module Command exposing (perform)

import Task


perform : msg -> Cmd msg
perform =
    Task.perform identity << Task.succeed
