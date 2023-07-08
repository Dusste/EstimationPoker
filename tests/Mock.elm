module Mock exposing (..)

import Types exposing (..)


mockTeamSize : Float
mockTeamSize =
    10


mockUsersSortedList : List User
mockUsersSortedList =
    [ { name = ValidTextField "User one"
      , isAdmin = False
      , card = Just 1
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User two"
      , isAdmin = False
      , card = Just 1
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User three"
      , isAdmin = False
      , card = Just 2
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User four"
      , isAdmin = False
      , card = Just 2
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User five"
      , isAdmin = False
      , card = Just 3
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User six"
      , isAdmin = False
      , card = Just 3
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User seven"
      , isAdmin = False
      , card = Just 4
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User eight"
      , isAdmin = False
      , card = Just 4
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User nine"
      , isAdmin = False
      , card = Just 5
      , sessionId = "123213233"
      , voteState = Voted
      }
    , { name = ValidTextField "User ten"
      , isAdmin = False
      , card = Just 5
      , sessionId = "123213233"
      , voteState = Voted
      }
    ]
