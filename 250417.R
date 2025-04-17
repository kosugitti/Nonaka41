rm(list=ls())
print("Hello, R World!")
5 + 3 * 2 - 10 / 2
(5 + 3) * (2 - 10) / 2
sqrt(841)
#??F1を押しながらsqrtを選択するか、?sqrtを実行する
2^3
98^7
sqrt(-4)
x<- 42
x
hoge <- 1:10
hoge
hoge2 <- 2*hoge
hoge2
str(hoge2)
#??Environmentタブで作成した変数を確認する
hoge2[3]
hoge2[2:5]
hoge2[c(2,4,6,8,10)]
matrix(hoge2, ncol=2)
hoge3 <- matrix(hoge2, ncol=2, byrow=TRUE)
hoge3
dim(hoge3)
hoge3[1,]
