module Reussir.Core2.Semi.Variable where

newVariable :: Identifier -> Maybe (Int64, Int64) -> Sem.Type -> Tyck Sem.VarID
newVariable varName varSpan varType = do
    st <- State.gets variableStates
    let varID = Sem.VarID $ fromIntegral $ Seq.length st
    let varDef = VarDef{varName, varSpan, varType}
    State.modify $ \s ->
        s
            { variableStates = st Seq.|> varDef
            }
    nameMap <- State.gets variableNameMap
    liftIO $ H.insert nameMap varName varID
    return varID

getVarType :: Sem.VarID -> Tyck Sem.Type
getVarType (Sem.VarID idx) = do
    vars <- State.gets variableStates
    let varDef = Seq.index vars (fromIntegral idx)
    return $ varType varDef

lookupVar :: Identifier -> Tyck (Sem.VarID, Sem.Type)
lookupVar varName = do
    nameMap <- State.gets variableNameMap
    liftIO (H.lookup nameMap varName) >>= \case
        Just varID -> getVarType varID >>= \ty -> pure (varID, ty)
        Nothing -> do
            reportError $ "Variable not found: " <> (unIdentifier varName)
            return (Sem.VarID (-1), Sem.TypeBottom)
