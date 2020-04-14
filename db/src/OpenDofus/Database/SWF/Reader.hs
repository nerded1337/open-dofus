-- Reader.hs ---

-- Copyright (C) 2019 Nerd Ed

-- Author: Nerd Ed <nerded.nerded@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}

module OpenDofus.Database.SWF.Reader
  ( loadData
  , unsafeString
  , unsafeInt
  , unsafeBool
  , unsafeArray
  , safeInt
  )
where

import           ByteCode
import qualified Data.Aeson                    as A
import           Data.Binary.Get
import qualified Data.ByteString.Lazy          as BS
import qualified Data.HashMap.Strict           as H
import           Data.List
import           Data.Scientific
import qualified Data.Sequence                 as S
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V
import           Data.Word
import           Parser
import           OpenDofus.Data.Constructible
import           OpenDofus.Prelude       hiding ( evaluate )
import           RIO.State

loadData :: FilePath -> (H.HashMap T.Text A.Value -> IO b) -> IO b
loadData filePath callback = do
  (A.Object x) <- loadFromFile filePath
  callback x

loadFromFile :: String -> IO A.Value
loadFromFile file = do
  codes <- getAbcFrom file
  let code' = traverse_ evaluate codes
  (_, e@(Evaluation _ tbl _ _ _)) <- runStateT
    code'
    (Evaluation mempty mempty mempty (const "IMPOSSIBLE") (const $ pure ()))
  (t, _) <- runStateT (traverse toAeson tbl) e
  pure $ A.Object t

getAbcFrom :: String -> IO (V.Vector ActionExpression)
getAbcFrom s = do
  (SwfFile_File _ _ _ _ _ _ tags) <- parseSwfFile s
  pure $ foldMap getTagInstructions tags

getTagInstructions :: Tag -> V.Vector ActionExpression
getTagInstructions (Tag_Opaque TagKind_DoAction _ content) =
  runGet getInstructions content
getTagInstructions _ = mempty

getInstructions :: Get (V.Vector ActionExpression)
getInstructions = getWhile isOpCodeValid decodeInstruction

data StackExpr
  = StackVal !PushExpression
  | StackObj !A.Value
  deriving (Show)

data Evaluation =
  Evaluation
    { pool        :: !(V.Vector T.Text)
    , table       :: !(H.HashMap T.Text StackExpr)
    , stack       :: !(S.Seq StackExpr)
    , getConstant :: Int -> T.Text
    , _toMember   :: A.Value -> StateT Evaluation IO ()
    }

evaluate :: ActionExpression -> StateT Evaluation IO ()
evaluate (Actions      a ) = traverse_ evaluate a
evaluate (ConstantPool p') = do
  p <- gets pool
  let p'' = p <> p'
  modify
    (\e ->
      e { pool = p'', getConstant = \i -> V.unsafeIndex p'' (V.length p + i) }
    )
evaluate (Push p) = modify (\e -> e { stack = StackVal p S.<| stack e })
evaluate (Operation operation) = do
  (Evaluation p tbl s idx toMember) <- get
  let getName push = case push of
        (PushConst8  i) -> idx $ fromIntegral i
        (PushConst16 i) -> idx $ fromIntegral i
        (PushString  n) -> n
        (PushDouble  n) -> T.pack $ show n
        x               -> error $ "Unhandled: " <> show x
  case (operation, s) of
    (ActionSetVariable, value :- (StackVal v :- xs)) -> do
      let name = getName v
          tbl' = H.insert name value tbl
      put (Evaluation p tbl' xs idx toMember)

    (ActionGetVariable, (StackVal v :- xs)) -> do
      let varName = getName v
          object =
            fromMaybe (StackObj $ A.Object mempty) $ H.lookup varName tbl
      put
        (Evaluation p tbl (object :- xs) idx $ \x ->
          modify (\e -> e { table = H.insert varName (StackObj x) (table e) })
        )

    (ActionGetMember, (StackVal (PushConst8 i)) :- (StackObj (A.Object o) :- xs))
      -> do
        let name = idx $ fromIntegral i
            o'   = StackObj $ fromMaybe (A.Object mempty) $ H.lookup name o
        put
          ( Evaluation p tbl (o' :- xs) idx
          $ \x -> toMember (A.Object $ H.insert name x o)
          )

    (ActionCallMethod, StackVal (PushConst8 _) :- (StackObj _ :- (StackVal (PushInt params) :- xs)))
      -> put
        (Evaluation p
                    tbl
                    (StackVal (PushInt 0) :- S.drop (fromIntegral params) xs)
                    idx
                    toMember
        )

    (ActionCallMethod, StackVal (PushConst8 _) :- (StackObj _ :- (StackVal (PushDouble params) :- xs)))
      -> put
        (Evaluation
          p
          tbl
          (StackVal (PushInt 0) :- S.drop (fromIntegral @Int $ round params) xs)
          idx
          toMember
        )

    (ActionCallFunction, (StackVal (PushString _)) :- (StackVal (PushInt params) :- xs@(value :- _)))
      -> do
        value' <- toAeson value
        put
          (Evaluation p
                      tbl
                      (StackObj value' :- S.drop (fromIntegral params) xs)
                      idx
                      toMember
          )

    (ActionCallFunction, (StackVal (PushString _)) :- (StackVal (PushDouble params) :- xs@(value :- _)))
      -> do
        value' <- toAeson value
        put
          (Evaluation
            p
            tbl
            (StackObj value' :- S.drop (fromIntegral @Int $ round params) xs)
            idx
            toMember
          )

    (ActionPop      , _ :- xs) -> put (Evaluation p tbl xs idx toMember)

    (ActionNewObject, StackVal name :- (StackVal (PushInt params) :- xs)) -> do
      let name'  = getName name
          object = StackObj $ case name' of
            "Object" -> A.Object mempty
            "Array"  -> A.Object mempty -- can access random index, same as object
            x        -> error $ "Unhandled: " <> show x
      put
        (Evaluation p
                    tbl
                    (object :- S.drop (fromIntegral params) xs)
                    idx
                    toMember
        )

    (ActionNewObject, StackVal name :- (StackVal (PushDouble params) :- xs)) ->
      do
        let name'  = getName name
            object = StackObj $ case name' of
              "Object" -> A.Object mempty
              "Array"  -> A.Object mempty
              x        -> error $ "Unhandled: " <> show x
        put
          (Evaluation p
                      tbl
                      (object :- S.drop (fromIntegral @Int $ round params) xs)
                      idx
                      toMember
          )

    (ActionInitArray, StackVal (PushInt nbOfElem) :- xs) -> do
      let (params, rest) = S.splitAt (fromIntegral nbOfElem) xs
      values <- traverse toAeson $ toList params
      put
        (Evaluation p
                    tbl
                    (StackObj (A.Array $ V.fromList values) :- rest)
                    idx
                    toMember
        )

    (ActionInitArray, StackVal (PushDouble nbOfElem) :- xs) -> do
      let (params, rest) = S.splitAt (fromIntegral @Int $ round nbOfElem) xs
      values <- traverse toAeson $ toList params
      put
        (Evaluation p
                    tbl
                    (StackObj (A.Array $ V.fromList values) :- rest)
                    idx
                    toMember
        )

    (ActionInitObject, StackVal (PushInt nbOfElem) :- xs) -> do
      let popArgument = do
            (Evaluation p' tbl' xs' idx' v') <- get
            case xs' of
              (val :- (StackVal varName :- xs'')) -> do
                let name = getName varName
                val' <- toAeson val
                put (Evaluation p' tbl' xs'' idx' v')
                pure (name, val')
              _ -> error $ "Unhandled: " <> show xs'
      put (Evaluation p tbl xs idx toMember)
      elems <- V.replicateM (fromIntegral nbOfElem) popArgument
      e     <- get
      let obj = StackObj $ A.Object $ V.foldr'
            (\(name, value) m -> H.insert name value m)
            H.empty
            elems
      put $! e { stack = obj :- stack e }

    (ActionInitObject, StackVal (PushDouble nbOfElem) :- xs) -> do
      let popArgument = do
            (Evaluation p' tbl' xs' idx' v') <- get
            case xs' of
              (val :- (StackVal varName :- xs'')) -> do
                let name = getName varName
                val' <- toAeson val
                put (Evaluation p' tbl' xs'' idx' v')
                pure (name, val')
              _ -> error $ "Unhandled: " <> show xs'
      put (Evaluation p tbl xs idx toMember)
      elems <- V.replicateM (fromIntegral @Int $ round nbOfElem) popArgument
      e     <- get
      let obj = StackObj $ A.Object $ V.foldr'
            (\(name, value) m -> H.insert name value m)
            H.empty
            elems
      put $! e { stack = obj :- stack e }

    (ActionSetMember, member :- (StackVal name :- (StackObj object :- xs))) ->
      do
        member' <- toAeson member
        let
          object' = case (name, object) of
            (PushConst8 i, A.Object o) ->
              A.Object $ H.insert (idx $ fromIntegral i) member' o
            (PushConst16 i, A.Object o) ->
              A.Object $ H.insert (idx $ fromIntegral i) member' o
            (PushString n, A.Object o) -> A.Object $ H.insert n member' o
            (PushInt    _, A.Array a ) -> A.Array $ V.snoc a member'
            (PushInt n, A.Object o) ->
              A.Object $ H.insert (T.pack $ show n) member' o
            (PushDouble _, A.Array a) -> A.Array $ V.snoc a member'
            (PushDouble n, A.Object o) ->
              A.Object $ H.insert (T.pack $ show n) member' o
            x -> error $ "Unhandled: " <> show x
        toMember object'
        (Evaluation p' tbl' _ _ _) <- get
        put (Evaluation p' tbl' xs idx (const $ pure ()))

    (ActionToString, _) -> pure ()

    (ActionToNumber, _) -> pure ()

    (x             , _) -> error $ "UNKNOWN ACTION: " <> show x

evaluate x = error $ "Unhandled action: " <> show x

pushToAeson :: PushExpression -> StateT Evaluation IO A.Value
pushToAeson PushNull       = pure A.Null
pushToAeson PushUndefined  = pure A.Null
pushToAeson (PushInt    i) = pure $ A.Number (fromIntegral i)
pushToAeson (PushString s) = pure $ A.String s
pushToAeson (PushDouble d) = pure $ A.Number (realToFrac d)
pushToAeson (PushFloat  f) = pure $ A.Number (realToFrac f)
pushToAeson (PushBool   b) = pure $ A.Bool b
pushToAeson (PushConst16 x) =
  A.String <$> (gets getConstant <*> pure (fromIntegral x))
pushToAeson (PushConst8 x) =
  A.String <$> (gets getConstant <*> pure (fromIntegral x))
pushToAeson (PushRegister _) = error "Unhandled register push"

toAeson :: StackExpr -> StateT Evaluation IO A.Value
toAeson (StackVal v) = pushToAeson v
toAeson (StackObj o) = pure o

unsafeArray :: String -> A.Value -> V.Vector A.Value
unsafeArray _ (A.Array x) = x
unsafeArray s x = error $ "Unhandled array type: " <> s <> ", " <> show x

unsafeString :: String -> A.Value -> T.Text
unsafeString _ (A.String x) = x
unsafeString s x = error $ "Unhandled string type: " <> s <> ", " <> show x

unsafeInt :: String -> A.Value -> Int
unsafeInt _ (A.Number x) = either (error "") id $ floatingOrInteger @Double x
unsafeInt s x            = error $ "Unhandled int type: " <> s <> ", " <> show x

unsafeBool :: String -> A.Value -> Bool
unsafeBool _ (A.Bool x) = x
unsafeBool s x          = error $ "Unhandled bool type: " <> s <> ", " <> show x

safeInt :: A.Value -> Maybe Int
safeInt (A.Number x) =
  Just $ either (error "Unhandled value") id $ floatingOrInteger @Double x
safeInt _ = Nothing

isOpCodeValid :: Get Bool
isOpCodeValid = do
  code <- lookAhead getWord8
  pure $! code /= 0

getWhile :: Get Bool -> Get b -> Get (V.Vector b)
getWhile f g = go mempty
 where
  go !k = do
    continue <- f
    case continue of
      False -> pure k
      True  -> do
        x <- g
        go (V.snoc k x)

decodeInstruction :: Get ActionExpression
decodeInstruction = getHeader getBody
 where
  loop f !offset = do
    offset' <- bytesRead
    if offset' < offset
      then do
        v <- f
        V.cons v <$> loop f offset
      else pure mempty

  getHeader !next = do
    code <- getWord8
    let action = inverseProjection instructionCode code
    if code < 0x80
      then next (action, 0)
      else do
        len    <- getWord16le
        offset <- bytesRead
        Actions <$> loop (next (action, fromIntegral len))
                         (offset + fromIntegral len)

  getBody (Just ActionConstantPool, _) = do
    count        <- getWord16le
    constantPool <- V.replicateM (fromIntegral count) decodeString
    pure $! ConstantPool constantPool

  getBody (Just ActionPush, _) = do
    code <- getWord8
    let pushType =
          fromMaybe (error $ "invalid push type: " <> show code)
            $ inverseProjection pushTypeCode code
    action <- case pushType of
      PushStringType     -> PushString <$> decodeString
      PushFloatType      -> PushFloat <$> getFloatle
      PushNullType       -> pure PushNull
      PushUndefinedType  -> pure PushUndefined
      PushRegisterType   -> PushRegister <$> getWord8
      PushBooleanType    -> PushBool . (== 1) <$> getWord8
      PushDoubleType     -> PushDouble <$> getDoublele
      PushIntegerType    -> PushInt <$> getInt32le
      PushConstant8Type  -> PushConst8 <$> getWord8
      PushConstant16Type -> PushConst16 <$> getWord16le
    pure $! Push action

  getBody (Just x, len) = do
    skip len
    pure $! Operation x

  getBody (Nothing, len) = do
    skip len
    pure Unknown

decodeString :: Get T.Text
decodeString = T.decodeUtf8 . BS.toStrict <$> getLazyByteStringNul

inverseProjection :: (Enum a, Bounded a) => (a -> Word8) -> Word8 -> Maybe a
inverseProjection project x =
  let domain = (,) <$> project <*> id
  in  fmap snd . find ((== x) . fst) $ domain <$> [minBound .. maxBound]

pushTypeCode :: PushType -> Word8
pushTypeCode PushStringType     = 0
pushTypeCode PushFloatType      = 1
pushTypeCode PushNullType       = 2
pushTypeCode PushUndefinedType  = 3
pushTypeCode PushRegisterType   = 4
pushTypeCode PushBooleanType    = 5
pushTypeCode PushDoubleType     = 6
pushTypeCode PushIntegerType    = 7
pushTypeCode PushConstant8Type  = 8
pushTypeCode PushConstant16Type = 9

data PushExpression
  = PushString {-# UNPACK #-}!T.Text
  | PushFloat {-# UNPACK #-}!Float
  | PushNull
  | PushUndefined
  | PushRegister {-# UNPACK #-}!Word8
  | PushBool !Bool
  | PushDouble {-# UNPACK #-}!Double
  | PushInt {-# UNPACK #-}!Int32
  | PushConst8 {-# UNPACK #-}!Word8
  | PushConst16 {-# UNPACK #-}!Word16
  deriving (Show, Eq)

data ActionExpression
  = ConstantPool {-# UNPACK #-}!(V.Vector T.Text)
  | Push !PushExpression
  | Actions {-# UNPACK #-}!(V.Vector ActionExpression)
  | Operation !DoActionInstruction
  | Unknown
  deriving (Show, Eq)

data PushType
  = PushStringType
  | PushFloatType
  | PushNullType
  | PushUndefinedType
  | PushRegisterType
  | PushBooleanType
  | PushDoubleType
  | PushIntegerType
  | PushConstant8Type
  | PushConstant16Type
  deriving (Eq, Show, Enum, Bounded)

data DoActionInstruction
  = ActionHasLength
  | ActionNone
  | ActionGotoFrame
  | ActionGetURL
  | ActionNextFrame
  | ActionPrevFrame
  | ActionPlay
  | ActionStop
  | ActionToggleQuality
  | ActionStopSounds
  | ActionWaitForFrame
  | ActionSetTarget
  | ActionGotoLabel
  | ActionAdd
  | ActionSubtract
  | ActionMultiply
  | ActionDivide
  | ActionEquals
  | ActionLess
  | ActionAnd
  | ActionOr
  | ActionNot
  | ActionStringEquals
  | ActionStringLength
  | ActionStringAdd
  | ActionStringExtract
  | ActionPush
  | ActionPop
  | ActionToInteger
  | ActionJump
  | ActionIf
  | ActionCall
  | ActionGetVariable
  | ActionSetVariable
  | ActionGetURL2
  | ActionGotoFrame2
  | ActionSetTarget2
  | ActionGetProperty
  | ActionSetProperty
  | ActionCloneSprite
  | ActionRemoveSprite
  | ActionTrace
  | ActionStartDrag
  | ActionEndDrag
  | ActionStringLess
  | ActionWaitForFrame2
  | ActionRandomNumber
  | ActionMBStringLength
  | ActionCharToAscii
  | ActionAsciiToChar
  | ActionGetTime
  | ActionMBStringExtract
  | ActionMBCharToAscii
  | ActionMBAsciiToChar
  | ActionDelete
  | ActionDefineFunction
  | ActionDelete2
  | ActionDefineLocal
  | ActionCallFunction
  | ActionReturn
  | ActionModulo
  | ActionNewObject
  | ActionDefineLocal2
  | ActionInitArray
  | ActionInitObject
  | ActionTypeOf
  | ActionTargetPath
  | ActionEnumerate
  | ActionStoreRegister
  | ActionAdd2
  | ActionLess2
  | ActionEquals2
  | ActionToNumber
  | ActionToString
  | ActionPushDuplicate
  | ActionStackSwap
  | ActionGetMember
  | ActionSetMember
  | ActionIncrement
  | ActionDecrement
  | ActionCallMethod
  | ActionNewMethod
  | ActionWith
  | ActionConstantPool
  | ActionStrictMode
  | ActionBitAnd
  | ActionBitOr
  | ActionBitXor
  | ActionBitLShift
  | ActionBitRShift
  | ActionBitURShift
  | ActionInstanceOf
  | ActionEnumerate2
  | ActionStrictEquals
  | ActionGreater
  | ActionStringGreater
  | ActionDefineFunction2
  | ActionTry
  | ActionThrow
  | ActionCastOp
  | ActionImplementsOp
  | ActionExtends
  | ActionNop
  | ActionHalt
  deriving (Show, Eq, Enum, Bounded)

instructionCode :: DoActionInstruction -> Word8
instructionCode ActionHasLength       = 0x80
instructionCode ActionNone            = 0x00
instructionCode ActionGotoFrame       = 0x81
instructionCode ActionGetURL          = 0x83
instructionCode ActionNextFrame       = 0x04
instructionCode ActionPrevFrame       = 0x05
instructionCode ActionPlay            = 0x06
instructionCode ActionStop            = 0x07
instructionCode ActionToggleQuality   = 0x08
instructionCode ActionStopSounds      = 0x09
instructionCode ActionWaitForFrame    = 0x8A
instructionCode ActionSetTarget       = 0x8B
instructionCode ActionGotoLabel       = 0x8C
instructionCode ActionAdd             = 0x0A
instructionCode ActionSubtract        = 0x0B
instructionCode ActionMultiply        = 0x0C
instructionCode ActionDivide          = 0x0D
instructionCode ActionEquals          = 0x0E
instructionCode ActionLess            = 0x0F
instructionCode ActionAnd             = 0x10
instructionCode ActionOr              = 0x11
instructionCode ActionNot             = 0x12
instructionCode ActionStringEquals    = 0x13
instructionCode ActionStringLength    = 0x14
instructionCode ActionStringAdd       = 0x21
instructionCode ActionStringExtract   = 0x15
instructionCode ActionPush            = 0x96
instructionCode ActionPop             = 0x17
instructionCode ActionToInteger       = 0x18
instructionCode ActionJump            = 0x99
instructionCode ActionIf              = 0x9D
instructionCode ActionCall            = 0x9E
instructionCode ActionGetVariable     = 0x1C
instructionCode ActionSetVariable     = 0x1D
instructionCode ActionGetURL2         = 0x9A
instructionCode ActionGotoFrame2      = 0x9F
instructionCode ActionSetTarget2      = 0x20
instructionCode ActionGetProperty     = 0x22
instructionCode ActionSetProperty     = 0x23
instructionCode ActionCloneSprite     = 0x24
instructionCode ActionRemoveSprite    = 0x25
instructionCode ActionTrace           = 0x26
instructionCode ActionStartDrag       = 0x27
instructionCode ActionEndDrag         = 0x28
instructionCode ActionStringLess      = 0x29
instructionCode ActionWaitForFrame2   = 0x8D
instructionCode ActionRandomNumber    = 0x30
instructionCode ActionMBStringLength  = 0x31
instructionCode ActionCharToAscii     = 0x32
instructionCode ActionAsciiToChar     = 0x33
instructionCode ActionGetTime         = 0x34
instructionCode ActionMBStringExtract = 0x35
instructionCode ActionMBCharToAscii   = 0x36
instructionCode ActionMBAsciiToChar   = 0x37
instructionCode ActionDelete          = 0x3A
instructionCode ActionDefineFunction  = 0x9B
instructionCode ActionDelete2         = 0x3B
instructionCode ActionDefineLocal     = 0x3C
instructionCode ActionCallFunction    = 0x3D
instructionCode ActionReturn          = 0x3E
instructionCode ActionModulo          = 0x3F
instructionCode ActionNewObject       = 0x40
instructionCode ActionDefineLocal2    = 0x41
instructionCode ActionInitArray       = 0x42
instructionCode ActionInitObject      = 0x43
instructionCode ActionTypeOf          = 0x44
instructionCode ActionTargetPath      = 0x45
instructionCode ActionEnumerate       = 0x46
instructionCode ActionStoreRegister   = 0x87
instructionCode ActionAdd2            = 0x47
instructionCode ActionLess2           = 0x48
instructionCode ActionEquals2         = 0x49
instructionCode ActionToNumber        = 0x4A
instructionCode ActionToString        = 0x4B
instructionCode ActionPushDuplicate   = 0x4C
instructionCode ActionStackSwap       = 0x4D
instructionCode ActionGetMember       = 0x4E
instructionCode ActionSetMember       = 0x4F
instructionCode ActionIncrement       = 0x50
instructionCode ActionDecrement       = 0x51
instructionCode ActionCallMethod      = 0x52
instructionCode ActionNewMethod       = 0x53
instructionCode ActionWith            = 0x94
instructionCode ActionConstantPool    = 0x88
instructionCode ActionStrictMode      = 0x89
instructionCode ActionBitAnd          = 0x60
instructionCode ActionBitOr           = 0x61
instructionCode ActionBitXor          = 0x62
instructionCode ActionBitLShift       = 0x63
instructionCode ActionBitRShift       = 0x64
instructionCode ActionBitURShift      = 0x65
instructionCode ActionInstanceOf      = 0x54
instructionCode ActionEnumerate2      = 0x55
instructionCode ActionStrictEquals    = 0x66
instructionCode ActionGreater         = 0x67
instructionCode ActionStringGreater   = 0x68
instructionCode ActionDefineFunction2 = 0x8E
instructionCode ActionTry             = 0x8F
instructionCode ActionThrow           = 0x2A
instructionCode ActionCastOp          = 0x2B
instructionCode ActionImplementsOp    = 0x2C
instructionCode ActionExtends         = 0x69
instructionCode ActionNop             = 0x77
instructionCode ActionHalt            = 0x5F
