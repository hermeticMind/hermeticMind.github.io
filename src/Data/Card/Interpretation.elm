module Data.Card.Interpretation exposing (fromCard)

import Data.Card as Card exposing (Card(..))
import Data.Symbol as Symbol exposing (Symbol(..))
import Html.Attributes exposing (list)
import List.Extra as List
import String.Extra as String


fromCard :
    Card
    -> ( { symbol : Symbol, text : String }, Maybe { symbol : Symbol, text : String } )
fromCard card =
    let
        default =
            ( { symbol = Square, text = "" }, Nothing )
    in
    case card of
        Binary int ->
            case int of
                1 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                2 ->
                    ( { symbol =
                            --Protection
                            Triangle
                      , text =
                            "Protect the ones you love and they will protect you. "
                                ++ "Make sure you give them some space and ask if they actually need your help. "
                                ++ "It might be that you are protecting something or someone who does not need protection."
                      }
                    , Just
                        { symbol = Square
                        , text =
                            "You are safe and protected. Nothing can hurt you and no one can come near you. "
                                ++ "But this secureness, also comes with restrictions. "
                                ++ "Get out of your shell. You have to take risks if you want change to occur. "
                                ++ "How knows, it might be that you realize afterwards how constrained you have been."
                        }
                    )

                _ ->
                    default

        Trump int ->
            case int of
                1 ->
                    ( { symbol =
                            --The Magician
                            Circle
                      , text =
                            "Your mind is your sword to split the chaos into clarity. "
                                ++ "All you need to get this clarity is a sharp and open mind. "
                                ++ "Get comfortable with the unknown. "
                                ++ "And the Unknown will present themselves to you."
                      }
                    , Nothing
                    )

                2 ->
                    ( { symbol =
                            --Intuition
                            Triangle
                      , text =
                            "Follow your hearth, it will lead to towards happiness."
                                ++ "Close your eyes, stop thinking and contemplating."
                                ++ "Let it happen and let it take you on a ride."
                      }
                    , Nothing
                    )

                5 ->
                    ( { symbol = Circle
                      , text =
                            "Your soul and spirit drive your body. "
                                ++ "They control it and guide it. "
                                ++ "Sometimes the pain you feel is overwhelming\u{00A0} your thoughts and hinders you to make the right decisions. "
                                ++ "Try to see the situation from outside your body. "
                                ++ "Ignore the pain for a moment on concentrate at what your soul wants to achieve."
                      }
                    , Nothing
                    )

                8 ->
                    ( { symbol =
                            --Justice
                            Square
                      , text =
                            "Life treats those who are good and punishes the ones who are bad. "
                                ++ "You will see justice. "
                                ++ "Straighten out all your flaws. "
                                ++ "Treat others the way you want to be treated."
                      }
                    , Nothing
                    )

                9 ->
                    ( { symbol =
                            --hermit
                            Circle
                      , text =
                            "You are on the search for something. "
                                ++ "Take your time and enjoy the moment. "
                                ++ "You will see that it is not the goal but the quest to find it, "
                                ++ "that holds the truth you're looking for."
                      }
                    , Nothing
                    )

                10 ->
                    ( { symbol =
                            -- Wheel of Fortune
                            Triangle
                      , text =
                            "The current situation might not be the way you wanted it, but stay fateful. "
                                ++ "Things will align to form the solution to your current problem. "
                                ++ "Don't point at others, don't try to blame someone for something. "
                                ++ "Be humble and be patient. You know that all will make sense at some point."
                      }
                    , Just
                        { symbol =
                            --fate
                            Circle
                        , text =
                            "Time is already written. "
                                ++ "What has happened and what will happened is not object to change. "
                                ++ "Embrace the path you're on and accept your fate."
                        }
                    )

                12 ->
                    ( { symbol = Square
                      , text =
                            --Calmness
                            "The everyday grind can be exhausting and stressful. "
                                ++ "You put up with the problems each day presents you with, day after day. "
                                ++ "Have you ever stopped and experienced the current moment you're in? "
                                ++ "Breath in and out. Take your time and try to take things more calmly. "
                                ++ "You will see that seemingly unsolvable problems with vanish in front of your eyes."
                      }
                    , Nothing
                    )

                15 ->
                    ( { symbol =
                            --The devil
                            Triangle
                      , text =
                            "todo"
                      }
                    , Just
                        { symbol = Square
                        , text =
                            "You are dependent to something or someone. "
                                ++ "It might be time to question this dependency. "
                                ++ "Is this dependency helpful to you? Does it make you happy? "
                                ++ "Sometimes the first step to change is to acknowledge that a problem exists."
                        }
                    )

                18 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                _ ->
                    default

        Element n ->
            case n of
                1 ->
                    ( { symbol =
                            --earth
                            Triangle
                      , text =
                            "Dreams are a way to play with ideas. They can also be used to find solutions to problems."
                                ++ "It's time to dream. Dream big, but don't lose reality out of sight."
                                ++ "Else you might lose yourself inside the dream."
                      }
                    , Nothing
                    )

                2 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                3 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                4 ->
                    ( { symbol =
                            --winter
                            Square
                      , text =
                            "Resources in the world are sparse in particular the resources you are given. "
                                ++ "You need to use them intelligently to harness your potential. "
                                ++ "You have the knowledge you need to use the tools you have at your disposal. "
                      }
                    , Nothing
                    )

                _ ->
                    default

        Planet n ->
            case n of
                1 ->
                    ( { symbol = Circle, text = "todo" }, Just { symbol = Square, text = "todo" } )

                2 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                3 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                4 ->
                    ( { symbol =
                            --Mars
                            Square
                      , text =
                            "Wealth is the pure essence of material matter. "
                                ++ "More wealth means more power and more possibilities. "
                                ++ "But wealth does not necessarily mean money. "
                                ++ "It can be also seen as a wealth of knowledge or ideas or even possibilities. "
                                ++ "You should appreciate what you already and make sure to exhausted all options before you search for something you. "
                      }
                    , Nothing
                    )

                5 ->
                    ( { symbol =
                            --Jupiter
                            Square
                      , text =
                            "We humans thrive on the search for fulfillment. "
                                ++ "Life might feel empty or without purpose. But that is just a feeling. "
                                ++ "Once you find your personal meaning of life, it will give you the light to shine. "
                      }
                    , Nothing
                    )

                6 ->
                    ( { symbol =
                            --Saturn
                            Square
                      , text =
                            "Life is hard, but we are all in it together. "
                                ++ "You would not be where you are now, if it was not for everyone around you. "
                                ++ "Acknowledge their contribution and tell them how thankful you are for having the around. "
                                ++ "They might even return your gesture. "
                                ++ "It is always easier to do the first step, then to wait for someone else to do it for you."
                      }
                    , Nothing
                    )

                7 ->
                    ( { symbol =
                            --Uranus
                            Square
                      , text =
                            "Whenever you're struggling, remember that your struggle will end at some point. "
                                ++ "If you give all you've got, then success is inevitable. "
                                ++ "But it might be that you are already succeeding, you are just not noticing it. "
                                ++ "Set goals you can actually accomplish. Before you know it you reached your original goal. "
                                ++ "You can do it."
                      }
                    , Nothing
                    )

                8 ->
                    ( { symbol =
                            --Neptune
                            Square
                      , text =
                            "Your obligations can sometimes limit your freedom. "
                                ++ "But this does not have to be. "
                                ++ "Sure you might have more freedom with fewer obligations, but having obligations also gives you a purpose. "
                                ++ "Find the middle ground, and you will see that you can have the best of both worlds."
                      }
                    , Nothing
                    )

                _ ->
                    default

        Virtue n ->
            case n of
                1 ->
                    ( { symbol =
                            --Compassion
                            Triangle
                      , text =
                            "Take a step back. Look around you. How are the people around you feeling? "
                                ++ "Try more to few things out of their eyes. "
                                ++ "They might have had a rough time. "
                                ++ "Help them with their problems and they will help you with yours."
                      }
                    , Nothing
                    )

                2 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                3 ->
                    ( { symbol =
                            --openness
                            Circle
                      , text =
                            "A change in perspective can help you find solutions to your problems. "
                                ++ "Open your mind and you will find things that you did not know existed. "
                                ++ "Do not fear the unknown, embrace it."
                      }
                    , Nothing
                    )

                4 ->
                    ( { symbol =
                            --Forgiveness
                            Triangle
                      , text =
                            "Forgive yourself and forgive others."
                                ++ "Everyone can make mistakes and everyone should get a second chance."
                                ++ "Don't spend to much time with past conflict."
                                ++ "Look into the future and towards to possibility that anyone can change."
                      }
                    , Nothing
                    )

                5 ->
                    ( { symbol =
                            --Patience
                            Triangle
                      , text =
                            "Give the people around you some space. "
                                ++ "Give give them time to think and let them make mistakes. "
                                ++ "But also give yourself some space. Do not get angry. "
                                ++ "Stay aware and provide help if needed."
                      }
                    , Just
                        { symbol = Square
                        , text =
                            "The greater things in life need time to develop."
                                ++ "Be patient. Give it time to breath, to prosper and to develop."
                                ++ "All you can do it to take a step back."
                                ++ "New perspectives might provide long awaited answers."
                        }
                    )

                6 ->
                    ( { symbol =
                            --Loyalty
                            Triangle
                      , text =
                            "Stay loyal to yourself and stay loyal to others. "
                                ++ "Know your way and stay true to your path. "
                                ++ "Know your friends and stay truthful to them as well."
                      }
                    , Nothing
                    )

                7 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                8 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                9 ->
                    ( { symbol =
                            --Self-knowledge
                            Triangle
                      , text =
                            "All change starts within. "
                                ++ "Get to know yourself. Be in peace with yourself. Respect yourself. "
                                ++ "Treat your body like a temple and bow down to it."
                      }
                    , Nothing
                    )

                10 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                11 ->
                    ( { symbol =
                            --Honesty
                            Triangle
                      , text =
                            "We all make mistakes and it is easy to lie about it. "
                                ++ "You need to be honest to the people around you. "
                                ++ "Then and only then you can point out the dishonesty of other people."
                      }
                    , Just
                        { symbol = Square
                        , text =
                            "We like to distort our capabilities. "
                                ++ "Try to be more honest to yourself and what you can do. "
                                ++ "Turn your weaknesses into strengths and stand by them. "
                                ++ "You can stand strong, if you have nothing to hide."
                        }
                    )

                ---
                12 ->
                    ( { symbol =
                            --Temperance
                            Square
                      , text =
                            "We live in a word of extremes: Having the best cars, the nicest clothes, the most profit. "
                                ++ "But by always focusing on the extreme, we stop appreciating what we already own. "
                                ++ "Being moderate with your wishes will make you happier."
                      }
                    , Nothing
                    )

                13 ->
                    ( { symbol =
                            --Humor
                            Circle
                      , text =
                            "Life can be ruthless but it does not need to be sad. "
                                ++ "Bring some humor in your life. It will help handling dealing with hard and unforgivable challenges. "
                                ++ "And once you've found your humor, make sure to also pass it on to others."
                      }
                    , Nothing
                    )

                14 ->
                    ( { symbol = Triangle, text = "todo" }, Nothing )

                15 ->
                    ( { symbol =
                            --Courage
                            Square
                      , text =
                            "Life can be risky. It is just too natural to be afraid to do the next step. "
                                ++ "But think about it, what is the worst thing that can happen. "
                                ++ "Having courage can make your life riskier but it can also make it more exiting. "
                                ++ "You will never know if your worries were justified if you're not risking anything."
                      }
                    , Nothing
                    )

                16 ->
                    ( { symbol =
                            --Diligence
                            Square
                      , text =
                            "Life can sometimes feel like an uphill battle, but remember this: "
                                ++ "Your diligence will someday bear fruits. "
                                ++ "Until that day comes, continue what you are doing. "
                                ++ "The people around you appreciate your hard work."
                      }
                    , Nothing
                    )

                _ ->
                    default

        Joker ->
            ( { symbol = Circle
              , text =
                    "You are free to do what you want and be who you want to be. "
                        ++ "Now is the time to formulate your deepest wish and make it a reality. "
                        ++ "Don't hold back be the person you always wanted to be."
              }
            , Just
                { symbol = Triangle
                , text =
                    "Do not have to care about others. "
                        ++ "Think more about yourself. "
                        ++ "Realize your goals and let the people around you be a part of it. "
                        ++ "Take the opportunity and lead the way."
                }
            )

        Back ->
            default