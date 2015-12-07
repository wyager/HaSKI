import Intermediate.Programs (evaluate, ababs)
main = mapM_ putChar $ evaluate ababs
