module Tests
  ( tests,
  )
where

import Data.Foldable
import Data.Function
import Data.List
import Task
import Test.Tasty.Hspec

tests :: Spec
tests = do
  describe "List Monoids" $ do
    it "FirstMonoid" $ do
      let getFirst = destructFirstMonoid . fold . fmap constructFirstMonoid
      getFirst [1 :: Int, 2, 3] `shouldBe` Just 1
      getFirst [] `shouldBe` (Nothing :: Maybe ())
      getFirst [True, False] `shouldBe` Just True
      getFirst [True, True, False] `shouldBe` Just True
      getFirst [1 .. 1000] `shouldBe` Just (1 :: Int)
      getFirst [100 .. 1000] `shouldBe` Just (100 :: Int)
      getFirst [6, 2, 7, 398, 1984, 383, -1020] `shouldBe` Just (6 :: Int)
    it "LastMonoid" $ do
      let getLast = destructLastMonoid . fold . fmap constructLastMonoid
      getLast [1 :: Int, 2, 3] `shouldBe` Just 3
      getLast [] `shouldBe` (Nothing :: Maybe ())
      getLast [1 .. 1000] `shouldBe` Just (1000 :: Int)
      getLast [100 .. 10000] `shouldBe` Just (10000 :: Int)
      getLast [6, 2, 7, 398, 1984, 383, -1020, 498] `shouldBe` Just (498 :: Int)
      getLast [True, False] `shouldBe` Just False
      getLast [True, True, False] `shouldBe` Just False
      getLast [True, True, False, True] `shouldBe` Just True
  describe "Todo List" $ do
    let createTasks :: [(String, Priority)] -> MyTaskManager
        createTasks = foldl (\t (a, b) -> createTask a b t) emptyTaskManager
        getPrioritySets :: MyTaskManager -> [[(Completion, String)]]
        getPrioritySets =
          fmap (sort . fmap (\(a, _, b) -> (a, b)))
            . groupBy ((==) `on` (\(_, p, _) -> p))
            . getPriorityList
    it "emptyTaskManager" $
      getPriorityList (emptyTaskManager :: MyTaskManager) `shouldBe` []
    it "createTask" $ do
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("aaa", Medium),
              ("AAAAAAAAAA", High)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "aaa")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
    it "toggleTaskCompletion" $ do
      getPrioritySets
        ( toggleTaskCompletion "AAAAAAAAAA" . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        `shouldBe` [ [(Completed, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( toggleTaskCompletion "AAAAAAAAAA" . toggleTaskCompletion "AAAAAAAAAA" . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( toggleTaskCompletion "a"
            . toggleTaskCompletion "not here"
            . toggleTaskCompletion "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(Completed, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(Completed, "a")]
                   ]
    it "removeTask" $ do
      getPrioritySets
        ( removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( removeTask "AAAAAAAAAA" . removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( removeTask "aaa" . removeTask "oh HaiMark"
            . removeTask "AAAAAAAAAA"
            . removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "a")]
                   ]
    it "modifyPriority" $ do
      getPrioritySets
        ( modifyPriority "aaa" High . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        `shouldBe` [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO"), (NotCompleted, "aaa")],
                     [(NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . toggleTaskCompletion "a"
            . modifyPriority "a" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [ (Completed, "a"),
                       (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "aaa")
                     ],
                     [(NotCompleted, "oh HaiMark")]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . modifyPriority "a" High
            . modifyPriority "a" High
            . modifyPriority "oh HaiMark" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [ (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "a"),
                       (NotCompleted, "aaa"),
                       (NotCompleted, "oh HaiMark")
                     ]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . modifyPriority "a" High
            . modifyPriority "nit here sihji" High
            . modifyPriority "oh HaiMark" Low
            . modifyPriority "a" High
            . modifyPriority "oh HaiMark" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [ (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "a"),
                       (NotCompleted, "aaa")
                     ],
                     [(NotCompleted, "oh HaiMark")]
                   ]
    it "renameTask" $ do
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . toggleTaskCompletion "a"
            . renameTask "oh HaiMark" "iz task"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(NotCompleted, "aaa"), (NotCompleted, "iz task")],
                     [(Completed, "a")]
                   ]
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . toggleTaskCompletion "iz task"
            . renameTask "oh HaiMark" "iz task"
            . renameTask "iz not here" "ajajaj"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        `shouldBe` [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(Completed, "iz task"), (NotCompleted, "aaa")],
                     [(NotCompleted, "a")]
                   ]
  describe "Coffee" $ do
    it "HasPrice Coffee" $ do
      price (Coffee 2 [OatMilk, Cinnamon]) `shouldBe` 2 + price OatMilk + price Cinnamon
      price (Coffee 2 [Cinnamon, Cinnamon]) `shouldBe` 2 + price Cinnamon + price Cinnamon
      price (Coffee 2 []) `shouldBe` 2
      price (Coffee 2 [Cinnamon, Cream, Cinnamon])
        `shouldBe` 2
        + price Cinnamon
        + price Cinnamon
        + price Cream
    it "chargeCoffee" $ do
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 199 69)
        `shouldBe` Just (DebitCard 112 69)
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 87 69)
        `shouldBe` Just (DebitCard 0 69)
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 86 69)
        `shouldBe` Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 0 69)
        `shouldBe` Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard (-2189) 69)
        `shouldBe` Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon, WhiteSugar]) (DebitCard 199 69)
        `shouldBe` Just (DebitCard 97 69)
      chargeCoffee (Coffee 2 [WhiteSugar, OatMilk, Cinnamon, WhiteSugar]) (DebitCard 199 69)
        `shouldBe` Just (DebitCard 82 69)
    let coffee = Coffee 2 [OatMilk, Cinnamon]
    it "payForCoffee" $ do
      payForCoffee (Customer [DebitCard 199 69] 0) coffee
        `shouldBe` Just (Card (DebitCard 112 69))
      payForCoffee (Customer [DebitCard 86 12, DebitCard 199 69] 2) coffee
        `shouldBe` Just (Card (DebitCard 112 69))
      payForCoffee (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
        `shouldBe` Just (Card (DebitCard 112 69))
      payForCoffee (Customer [DebitCard 87 8, DebitCard 111 12, DebitCard 199 69] 0) coffee
        `shouldBe` Just (Card (DebitCard 0 8))
      payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 0) coffee
        `shouldBe` Nothing
      payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 87) coffee
        `shouldBe` Just (Cash 0)
      payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 81 69] 86) coffee
        `shouldBe` Nothing
    it "applyPayment" $ do
      applyPayment (Customer [DebitCard 199 69] 0) (Card (DebitCard 112 69))
        `shouldBe` Customer [DebitCard 112 69] 0
      applyPayment (Customer [DebitCard 111 12, DebitCard 199 69] 2) (Card (DebitCard 112 69))
        `shouldBe` Customer [DebitCard 111 12, DebitCard 112 69] 2
      applyPayment (Customer [DebitCard (-12) 8, DebitCard 111 12, DebitCard 199 69] 0) (Card (DebitCard 112 69))
        `shouldBe` Customer [DebitCard (-12) 8, DebitCard 111 12, DebitCard 112 69] 0
      applyPayment (Customer [DebitCard 112 8, DebitCard 111 12, DebitCard 199 69] 0) (Card (DebitCard 0 8))
        `shouldBe` Customer [DebitCard 0 8, DebitCard 111 12, DebitCard 199 69] 0
      applyPayment (Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112) (Cash 0)
        `shouldBe` Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 0
      applyPayment (Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112) (Card (DebitCard 0 13))
        `shouldBe` Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112
    it "buyCoffee" $ do
      buyCoffee (Customer [DebitCard 199 69] 0) coffee
        `shouldBe` Just (Customer [DebitCard 112 69] 0)
      buyCoffee (Customer [DebitCard 86 12, DebitCard 199 69] 2) coffee
        `shouldBe` Just (Customer [DebitCard 86 12, DebitCard 112 69] 2)
      buyCoffee (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
        `shouldBe` Just (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 112 69] 0)
      buyCoffee (Customer [DebitCard 87 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
        `shouldBe` Just (Customer [DebitCard 0 8, DebitCard 86 12, DebitCard 199 69] 0)
      buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 84 69] 0) coffee
        `shouldBe` Nothing
      buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 87) coffee
        `shouldBe` Just (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 0)
      buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 81 69] 86) coffee
        `shouldBe` Nothing
    it "saveTheDiabetic" $ do
      saveTheDiabetic
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        `shouldBe` ( 150,
                     [ Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon]
                     ]
                   )
      saveTheDiabetic
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon, BrownSugar, SoyMilk],
          Coffee 0 [WhiteSugar, BrownSugar, WhiteSugar, WhiteSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        `shouldBe` ( 215,
                     [ Coffee 0 [Cinnamon, SoyMilk],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon]
                     ]
                   )
      saveTheDiabetic
        [ Coffee 0 [Cinnamon, SoyMilk],
          Coffee 0 [Cinnamon],
          Coffee 0 [Cinnamon],
          Coffee 69 []
        ]
        `shouldBe` ( 0,
                     [ Coffee 0 [Cinnamon, SoyMilk],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon],
                       Coffee 69 []
                     ]
                   )
      saveTheDiabetic
        []
        `shouldBe` ( 0,
                     []
                   )
    it "calculateSugarDanger" $ do
      calculateSugarDanger
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        `shouldBe` 9
      calculateSugarDanger
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon, BrownSugar, SoyMilk],
          Coffee 0 [WhiteSugar, BrownSugar, WhiteSugar, WhiteSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 []
        ]
        `shouldBe` 14
      calculateSugarDanger
        [ Coffee 0 [Cinnamon, SoyMilk],
          Coffee 0 [Cinnamon],
          Coffee 0 [Cinnamon]
        ]
        `shouldBe` 0
      calculateSugarDanger
        []
        `shouldBe` 0
