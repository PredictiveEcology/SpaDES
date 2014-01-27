# speed comparison of various R functions

-------------------
| FASTER | SLOWER |
-------------------
| `cbind.data.frame()` | `(as.)data.frame(cbind())` |
| `library(data.table); as.data.frame(rbindlist())` | `do.call(rbind,)` |
| `seq_len(X)` | `1:X` faster than `seq(1,X)` faster than `seq(1,X,by=1)` |
-------------------
