import System.Process
import System.IO

-- Remove any duplicates from a list.
makeUnique :: Eq a => [a] -> [a]
makeUnique [] = []
makeUnique (x:xs) = x : filter (/= x) (makeUnique xs)

data BuildFile =
  BuildFile {
    path      :: String,
    name      :: String,
    extension :: String
  }

makeFullPath :: BuildFile -> String
makeFullPath file = " " ++ (path file) ++ (name file) ++ (extension file)

-- Information on a single source file; includes -iquote and -l info.
data SourceFile =
  SourceFile {
    sourceFile      :: BuildFile,
    includes        :: [String],
    sourceLibraries :: [String]
  }

-- Result of a single source compilation. Include any messages to output and
-- the names of objects and libraries needed to link.
data ObjectFile =
  ObjectFile {
    objectFile      :: BuildFile,
    objectLibraries :: [String],
    objectProcess   :: CreateProcess
  }

-- Compile a list of sources to objects.
sourceToObject :: SourceFile -> BuildFile
sourceToObject source = BuildFile "build/" ((name . sourceFile) source) ".o"

extractIncludeList :: SourceFile -> String
extractIncludeList = concat . (map ("-iquote " ++)) . includes

compile    :: SourceFile   -> ObjectFile
compileAll :: [SourceFile] -> [ObjectFile]

compile source =
  let object = sourceToObject source
      sourceOption = " -c" ++ ((makeFullPath . sourceFile) source)
      objectOption = " -o" ++ (makeFullPath object)
      includeOptions = extractIncludeList source
      p = shell ("gcc" ++ sourceOption ++ objectOption ++ includeOptions)
  in ObjectFile object (sourceLibraries source) p

compileAll = map compile
   
data ExecutableFile =
  ExecutableFile {
    executableFile    :: BuildFile,
    executableProcess :: CreateProcess
  }

-- Shell call creation.
-- Begin by extracting all the object files and libraries needed in the linkage
-- then concatenate them together into a string and call the shell function.

objectsToExecutable :: [ObjectFile] -> BuildFile
objectsToExecutable objects = BuildFile "" "" ((name . objectFile) (objects !! 0))

extractObjectList :: [ObjectFile] -> String
extractObjectList = concat . (map (makeFullPath . objectFile))

extractLibraryList :: [ObjectFile] -> [[String]]
extractLibraryList = map objectLibraries

organizeLibraryList :: [[String]] -> [String]
organizeLibraryList = makeUnique . concat

flattenLibraryList :: [String] -> String
flattenLibraryList = concat . (map (" -l" ++))

link :: [ObjectFile] -> ExecutableFile
link objects =
  let executable = objectsToExecutable objects
      objectOptions = extractObjectList objects
      executableOption = " -o" ++ makeFullPath executable
      libraryList = (organizeLibraryList . extractLibraryList) objects
      libraryOptions = flattenLibraryList libraryList
      p = shell ("gcc" ++ objectOptions ++ executableOption ++ libraryOptions)
  in ExecutableFile executable p

-- Run an individual system call and extract the STDOUT handle.
runAProcess :: CreateProcess -> IO Handle
runAProcess p = do
  (_, Just hout, _, pid) <- createProcess (p{std_out = CreatePipe})
  waitForProcess pid
  return hout

-- List of all source files to compile.
theSauces :: [SourceFile]
theSauces =
  [ SourceFile (BuildFile "" "test" ".c") [] [] ]

main :: IO ()
main = 
  let objects = compileAll theSauces
      executable = link objects
      objectProcesses = map objectProcess objects
      exprocess = executableProcess executable
  in do
    runCompileProcesses objectProcesses
    runAProcess exprocess >>= print
    return ()
      where runCompileProcesses (x:xs) = (runAProcess x >>= print) >>
                                         runCompileProcesses xs
            runCompileProcesses []     = return ()

