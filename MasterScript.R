#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# MasterScript.R
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=

rm(list = ls()) # clear all

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 2. データの読み込み ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## 2.1 CSVファイルの読み込み ----
CensusDir <- "CensusCSV/CSV_EduWH/"
FN_EduWH1980csv <- paste0(CensusDir, "EduWH1980.csv")

EduWH1980_raw <- read.csv(file = FN_EduWH1980csv, header = FALSE) # "file = "は省略可

# CSVファイルを読み込んだ直後の型はデータフレームになっている
class(EduWH1980_raw) 

# as.matrix()でオブジェクトの型を行列に変換
EduWH1980_mat <- as.matrix(EduWH1980_raw)

class(EduWH1980_mat)

EduWH1980_mat

EduWH1980 <- EduWH1980_mat
class(dimnames(EduWH1980))

dimnames(EduWH1980)

dimnames(EduWH1980) <- list(c("JHS", "HS", "VC/JC", "UNI"),
                            c("JHS", "HS", "VC/JC", "UNI"))

EduWH1980

names(dimnames(EduWH1980)) <- c("EduW", "EduH")

# 完成!
EduWH1980

## 2.2 CSVファイルの出力 ----
### 2.2.1 出力するオブジェクトの準備 ----
# パラメータの設定
SupTabsDir <- "CensusCSV/SupplementalTables/" # 全体パーセントのCSVファイルが保存されているディレクトリ
OutDir     <- "CensusCSV/CSV_EduWH/"          # 出力先ディレクトリ
NofObs <- 481536　　# 全体度数

# 全体パーセントの読み込み
TotProp2010_raw <- read.csv(paste0(SupTabsDir, "TotProp2010.csv"), header = FALSE)
TotProp2010 <- as.matrix(TotProp2010_raw)
TotProp2010

# セル度数の算出
EduWH2010 <- NofObs * (TotProp2010 / 100)
EduWH2010

### 2.2.2 write.csv() ----
# write.csv(EduWH2010, file = paste0(OutDir, "EduWH2010_writecsv.csv"), row.names = FALSE)

### 2.2.3 write.table() ----
# write.table(EduWH2010, file = paste0(OutDir, "EduWH2010.csv"), sep = ",", row.names = FALSE, col.names = FALSE)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 3. 二元クロス集計表の処理 ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## 3.1 周辺度数の追加 ----
# 行周辺度数
addmargins(EduWH1980, margin = 1)

# 列周辺度数
addmargins(EduWH1980, margin = 2)

# 行／列周辺度数
addmargins(EduWH1980, margin = c(1,2))

## 3.2 比率（proportion）の算出 ----
### 3.2.1 proportions() ----
proportions(EduWH1980, margin = 1) * 100 # 行パーセント
proportions(EduWH1980, margin = 2) * 100 # 列パーセント

# 有効桁数を減らしたい場合は"round"で丸めるとよい
round(proportions(EduWH1980, margin = 1) * 100, digits = 1) # "digits = "は省略してもよい

proportions(EduWH1980, margin = "EduW")
proportions(EduWH1980, margin = "EduH")

EduWH1980_wRMar <- addmargins(EduWH1980, margin = 1)  # w/ row margins
round(proportions(EduWH1980_wRMar, margin = 1) * 100, digits = 1)

EduWH1980_wCMar <- addmargins(EduWH1980, margin = 2)  # w/ column margins
round(proportions(EduWH1980_wCMar, margin = 2) * 100, digits = 1)

### 3.2.2 rowPercents()・colPercents() ----
library(RcmdrMisc)
rowPercents(EduWH1980, digits = 1)
colPercents(EduWH1980, digits = 1)

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 4. 多元クロス集計表の処理 ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## 4.1 配列（array） ----
# 2×2×2の3次元からなり、要素がすべてNAの空の配列
array(data = NA, dim = c(2,2,2))

# 任意のベクトルを引き渡してもよい。列方向に順番に格納されていく。
array(data = 1:8, dim = c(2,2,2))

# データとして引き渡したベクトルの長さが次元の長さの積と等しくない場合、配列が埋まるまでデータのベクトルが反復される
array(data = 1:3, dim = c(2,2,2))

## 4.2 abind()を用いた配列の作成 ----
EduWH1980_raw <- read.csv(paste0(CensusDir, "EduWH1980.csv"), header = FALSE)
EduWH1990_raw <- read.csv(paste0(CensusDir, "EduWH1990.csv"), header = FALSE)

EduWH1980 <- as.matrix(EduWH1980_raw)
EduWH1990 <- as.matrix(EduWH1990_raw)

dimnames(EduWH1980) <- 
dimnames(EduWH1990) <- list(c("JHS", "HS", "VC/JC", "UNI"),
                            c("JHS", "HS", "VC/JC", "UNI"))

names(dimnames(EduWH1980)) <-
names(dimnames(EduWH1990)) <- c("EduW", "EduH")

readEduWH <- function(path, census_year){
  TargetCSV <- paste0(path, "EduWH", as.character(census_year), ".csv")  # 例えば1980年のデータファイル名は"EduWH1980.csv"にしておく
  EduWH_raw <- read.csv(TargetCSV, header = FALSE)
  EduWH_mat <- as.matrix(EduWH_raw)
  EduWH <- EduWH_mat
  
  dimnames(EduWH) <- list(c("JHS", "HS", "VC/JC", "UNI"),
                          c("JHS", "HS", "VC/JC", "UNI"))
  
  return(EduWH)
}

readEduWH(path = CensusDir, census_year = 1980)

library(abind)
CensusYearList <- seq(1980, 2010, 10)  # 国勢調査の調査年が格納されたベクトル

EduWH <- NULL  # 空のオブジェクトを作っておく
for(iYear in CensusYearList){
  EduWH_temp <- readEduWH(path = CensusDir, census_year = iYear)  # iYear年の国勢調査のデータを読み込む
  EduWH <- abind(EduWH, EduWH_temp, along = 3)  # その結果を事前に作っておいたEduWHに順次結合していく
}

dimnames(EduWH)[[3]] <- CensusYearList  # abindで結合した次元にはラベルがついていないのでつけておくと便利

names(dimnames(EduWH)) <- c("EduW", "EduH", "CensusYear") # 各次元の変数名を付与

EduWH

## 4.3 apply()を用いた配列の処理 ----
apply(EduWH, MARGIN = 3, FUN = proportions, simplify = FALSE)　# "MARGIN =", "FUN = "の部分は省略してもよい

apply(EduWH, "CensusYear", proportions, simplify = FALSE)

TotProp <- apply(EduWH, 3, proportions, simplify = FALSE)
class(TotProp)

TotProp$`1980`

## 4.4 apply()と自作関数を組み合わせる ----
ObsDiag  <- diag(EduWH1980)  # diag():行列の対角成分を取得
PropDiag <- ObsDiag / sum(EduWH1980)

prop_diag <- function(x){
  ObsDiag  <- diag(x)
  PropDiag <- ObsDiag / sum(x)
  return(PropDiag)
}

PropDiag_byCensus <- apply(EduWH, 3, prop_diag)
PropDiag_byCensus

#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
# 5. Data Visualization ----
#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=
## 5.1 棒グラフ ----
PropDiag_byCensus
barplot(PropDiag_byCensus)

barplot(PropDiag_byCensus, 
        ylim = c(0, 1),
        axes = FALSE,
        legend = rownames(PropDiag_byCensus))
axis(2, las = 1)
mtext("Census Year", side = 1, line = 3)
mtext("Proportions", side = 2, line = 3)
title("Proportions of Homogamy, by Education and Census Year")

barplot(PropDiag_byCensus,  beside = TRUE,
        ylim = c(0, 1),
        axes = FALSE,
        legend = rownames(PropDiag_byCensus))
axis(2, las = 1)
mtext("Census Year", side = 1, line = 3)
mtext("Proportions", side = 2, line = 3)
title("Proportions of Homogamy, by Education and Census Year")

## 5.2 折れ線グラフ ----
# 調査年ごとに同類婚の合計比率を算出
PropHomg <- apply(PropDiag_byCensus, 2, sum)
PropHomg

# 折れ線グラフを描く
plot(x = seq(1980, 2010, 10), #"x="は省略してもよい
     y = PropHomg,            #"y="は省略してもよい
     type = "o",
     pch  = 16,
     xlim = c(1980, 2010),
     ylim = c(0, 1),
     ann = FALSE,
     axes = FALSE)
axis(1)
axis(2, at = seq(0, 1, 0.25), las = 1)
mtext("Census Year", side = 1, line = 3)
mtext("Proportions", side = 2, line = 3)
title("Proportions of Homogamy by Census Year")

matplot(seq(1980, 2010, 10), t(PropDiag_byCensus),
        type = "o",
        lty = 1,
        pch = 16,
        xlim = c(1980, 2010),
        ylim = c(0, 0.5),
        ann = FALSE,
        axes = FALSE)
axis(1, at = seq(1980, 2010, 10))
axis(2, at = seq(0, 0.5, 0.1), las = 1)
mtext("Census Year", side = 1, line = 3)
mtext("Proportions", side = 2, line = 3)
legend("topright",
       rownames(PropDiag_byCensus),
       lty = 1,
       pch = 16,
       col = 1:4,
       bty = "n")

## 5.3 補足：クロス集計表の可視化 ----
### 5.3.1 モザイクプロット ----
mosaicplot(t(EduWH1980), main = "Distribution of Educational Pairings: 1980 Census")

### 5.3.2 バルーンプロット ----
library(gplots)
EduWH1980_tab <- as.table(EduWH1980)  # tableクラスにしてから引き渡す必要がある
balloonplot(x = t(EduWH1980_tab),
            main = "",
            ylab = "Wife's Education",
            xlab = "Husband's Education")

