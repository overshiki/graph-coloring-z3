module Main where
import Coloring
import Z3Engine

main :: IO ()
main = do
  printEval exampleProblem
  printSolve exampleProblem
  printSolveMatch exampleProblem
  let
    coloringProblem = buildColoringGC exampleGC
  printEval coloringProblem
  printSolve coloringProblem
  printSolveMatch coloringProblem

  -- let
  --   size = 3
  --   colorSize = 20
  -- edges <- randomGraph size
  -- let
  --   coloringProblem = buildColoring size colorSize edges
  -- printEval coloringProblem
  -- printSolve coloringProblem
  -- printSolveMatch coloringProblem
