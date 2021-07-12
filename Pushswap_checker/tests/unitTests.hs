import Test.HUnit
import Commands
import Parser

tests_commands :: Test
tests_commands = 
    TestList [
        "swap" ~: [2, 1] ~=? (swap [1, 2]),
        "swap : empty list" ~: [] ~=? (swap []),
        "pa" ~: ([1, 2, 3], [2, 3]) ~=? (pa ([2, 3], [1, 2, 3])),
        "pa : empty list"  ~: ([1, 2, 3], []) ~=? (pa ([1, 2, 3], [])),
        "pb" ~: ([2, 3], [1, 2, 3]) ~=? (pb ([1, 2, 3], [2, 3])),
        "pb : empty list" ~: ([], [1, 2, 3]) ~=? (pb ([], [1, 2, 3])),
        "r_ab" ~: [1, 2, 3] ~=? (r_ab [3, 1, 2]),
        "r_ab : empty list" ~: [] ~=? (r_ab []),
        "r_ab : one number" ~: [5] ~=? (r_ab [5]),
        "rr_ab" ~: [1, 2, 3] ~=? (rr_ab [2, 3, 1]),
        "rr_ab : empty list" ~: [] ~=? (rr_ab []),
        "rr_ab : one number" ~: [5] ~=? (rr_ab [5])
    ]

tests_errorHandling :: Test
tests_errorHandling =
    TestList [
        "checkNumber : positive number" ~: True ~=? (checkNumber "45"),
        "checkNumber : negative number" ~: True ~=? (checkNumber "-6"),
        "checkNumber : not a number" ~: False ~=? (checkNumber "kjfbzuif")
    ]

test_listSorted :: Test
test_listSorted =
    TestList [
        "isListSorted : yes all pos" ~: True ~=? (isListSorted [1, 2, 3]),
        "isListSorted : no all pos" ~: False ~=? (isListSorted [1, 3, 2]),
        "isListSorted : yes all neg" ~: True ~=? (isListSorted [(-3), (-2), (-1)]),
        "isListSorted : no all neg" ~: False ~=? (isListSorted [(-3), (-1), (-2)])
    ]

main :: IO Counts
main = 
    runTestTT tests_commands >>
    runTestTT tests_errorHandling >>
    runTestTT test_listSorted