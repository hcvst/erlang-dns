-module(ed_utils).
-compile(export_all).

tails([]) -> [[]];
tails([_|T]=Xs) -> [Xs|tails(T)].