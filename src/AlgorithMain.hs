module AlgorithmMain where




rgepMain fitnessFunction = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Get configuration files -}
  config <- loadConfiguration $ configFiles options

  pm   <- C.lookupDefault 0.001 config "pm"
  pr   <- C.lookupDefault 0.6   config "pr"
  pc1  <- C.lookupDefault 0.6   config "pc1"
  pc2  <- C.lookupDefault 0.6   config "pc2"
  gens <- C.lookupDefault 100   config "gens"
  ps   <- C.lookupDefault 50    config "ps"
  is   <- C.lookupDefault 100   config "is"

  {- Run algorithm -}
  let ops = polyOps
  pop <- rIO $ rgep ps is ops pm pr pc1 pc2 pt gens 0 fitnessFunction
  print pop

gaMain fitnessFunction = do
  {- Get command line options -}
  options <- execParser (info (helper <*> parseOptions) fullDesc)

  {- Get configuration files -}
  config <- loadConfiguration $ configFiles options

  pm   <- C.lookupDefault 0.001 config "pm"
  pc   <- C.lookupDefault 0.6   config "pc"
  gens <- C.lookupDefault 100   config "gens"
  ps   <- C.lookupDefault 50    config "ps"
  is   <- C.lookupDefault 100   config "is"

  {- Run algorithm -}
  --pop <- runApp config $ geneticAlgorithmApp ones
  pop <- rIO $ pipedGA ps is gens pm pc fitnessFunction
  --pop <- rIO $ parallelGA 50 1000 100 0.01 0.6 ones
  print pop 
  --result <- rIO . runStage' 0 . cycleNTimes 1000 $ simpleStage 
  --print result

loadConfiguration configs =
  let configs' = mainCfgFileName : defaultCfgName : map Required (P.filter (/= "") configs)
  in load configs'


