module Mock exposing (..)

import Types exposing (..)


mockTeamSize : Float
mockTeamSize =
    10


mockUsersSortedList : List User
mockUsersSortedList =
    [ { name = ValidTextField "User one"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 1
      }
    , { name = ValidTextField "User two"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 1
      }
    , { name = ValidTextField "User three"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 2
      }
    , { name = ValidTextField "User four"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 2
      }
    , { name = ValidTextField "User five"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 3
      }
    , { name = ValidTextField "User six"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 3
      }
    , { name = ValidTextField "User seven"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 4
      }
    , { name = ValidTextField "User eight"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 4
      }
    , { name = ValidTextField "User nine"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 5
      }
    , { name = ValidTextField "User ten"
      , isAdmin = False
      , sessionId = "123213233"
      , voteState = Voted 5
      }
    ]
