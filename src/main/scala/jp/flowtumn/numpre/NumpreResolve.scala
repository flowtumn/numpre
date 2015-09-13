package jp.flowtumn.numpre

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

trait SolverResult
object SolverSuccess extends SolverResult
object SolverNotFound extends SolverResult
object SolverImpossible extends SolverResult
object SolverInvalidParameter extends SolverResult
object SolverFail extends SolverResult

/**
 * 解法スキャンの実行を持つtrait.
 */
trait NumpreStrategy {
    /**
     * 標準実装は、単純にそのマスに埋まるべき値を列挙する。
     */
    def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, NumpreElementCandidate]
}

/**
 * Numpreの情報を持ったtrait.
 */
trait NumpreInfo {
    /**
     * 横幅。
     */
    def width: Int

    /**
     * 縦幅。
     */
    def height: Int

    /**
     * Rgnの横幅。
     */
    def rgnWidth: Int

    /**
     * Rgnの縦幅。
     */
    def rgnHeight: Int

    /**
     * 対角線。
     */
    def diagonal: Boolean
}

/**
 * Numpreの詳細の情報を持っているtrai.
 */
trait NumpreDetail extends NumpreInfo {
    /**
     * そのマスの値を取得。
     */
    def atValue(x: Int, y: Int): Int

    /**
     * Success or Impossibleを返します。
     */
    def getResult(strategy: NumpreStrategy): SolverResult = {
        scanInternal(0, 0, 0, strategy)
    }

    @tailrec
    private def scanInternal(x: Int, y: Int, candidate: Int, strategy: NumpreStrategy): SolverResult = {
        var xx = x
        var yy = y
        var can = candidate
        strategy.scan(x, y, this) match {
            case Right(v) =>
                //候補の数を加算
                can = can + v.values.size

                //次の座標へ
                val f = (vv: Int, max: Int) => {
                    if (vv + 1 <= max) {
                        0
                    } else {
                        vv + 1
                    }
                }

                xx = f(x, this.width)
                yy = f(y, this.height)
            case Left(r) =>
                return r
        }

        if ((xx <= this.width - 1) && (yy <= this.width - 1)) {
            if (0 == can) {
                //解法済み。
                SolverSuccess
            } else {
                //まだ解法は見つかっていない。
                SolverNotFound
            }
        } else {
            scanInternal(xx, yy, can, strategy)
        }
    }
}

class NumpreResolve(private val info: NumpreInfo, val initData: Iterable[NumpreElement]) {
    private val repository = new AtomicReference[NumpreRepository](OnMemoryRepositoryFactory.create(initData))
    private val table = Array.ofDim[Int](info.width, info.height)

    def solver: Either[Exception, SolverResult] = {
        // repositoryからtableを復元。
        repository.get.toIterable.foreach(
            each => {
                table(each.x)(each.y) = each.value
            }
        )

        for (y <- 0 until info.width) {
            for (x <- 0 until info.height) {
                print(table(x)(y))
            }
            println
        }

        val datas = scanAll(
            new NumpreDetail {
                override def width: Int = info.width
                override def height: Int = info.height
                override def rgnWidth: Int = info.rgnWidth
                override def rgnHeight: Int = info.rgnHeight
                override def diagonal: Boolean = info.diagonal
                override def atValue(x: Int, y: Int): Int = {
                    //0から数えるので -1
                    table(x)(y) - 1
                }
            })

        datas.foreach(
           each => {
                val f = (v: Iterable[Int]) => {
                    v.foreach(
                        each => {
                            print(each + " ")
                        }
                    )
                }
                print("x:   " + each.x + "    y: " + each.y + "  values: ")
                f(each.values)
                println("")
            }
        )

        Right(SolverSuccess)
    }

    /**
     * 全マスの候補を取得。
     */
    def scanAll(detail: NumpreDetail): Iterable[NumpreElementCandidate] = {
        val result = new scala.collection.mutable.Queue[NumpreElementCandidate]

        for (x <- 0 until detail.width) {
            for (y <- 0 until detail.height) {
                scan(x, y, detail) match {
                    case Right(v) =>
                        result.enqueue(v)
                    case Left(e) =>
                        // Noneなら候補は無い。(このループでNoneが返却されるのはDetailの実装ミス)
                        result.enqueue(new NumpreElementCandidate(x = x, y = y, values = Iterable[Int]()))
                }
            }
        }

        result
    }

    /**
     * 指定した座標から候補になる値全てを列挙する。更に解法が見つけられるのかも判断する。
     */
    def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, NumpreElementCandidate] = {
        val size = detail.height
        val candidate = new scala.collection.mutable.Queue[Int]
        var temp: Array[Int] = Array.fill(size)(-1);

        if (0 <= x && x < size && 0 <= y && y < size) {
            //この座標の値が空なら。
            if (-1 >= detail.atValue(x, y)) {
                //縦ラインのスキャン
                for (yy <- 0 until size) {
                    count(temp, x, yy, detail)
                }

                //横ラインのスキャン
                for (xx <- 0 until size) {
                    count(temp, xx, y, detail)
                }

                //対角線も揃えるか？
                if (detail.diagonal) {
                    //対角線も調べる。
                    if (y == x) {
                        for (i <- 0 until size) {
                            count(temp, i, i, detail)
                        }
                    }

                    //反対の対角線を調べる。
                    if ((x + y) == (size + 1)) {
                        for (i <- 0 until size) {
                            count(temp, size - i - 1, i, detail)
                        }
                    }
                }

                //リージョン位置を取得。
                val (rgnX, rgnY) = getRgn(x, y, detail)

                //リージョンを調べる。
                for (yy <- rgnY until rgnY + detail.rgnHeight) {
                    for (xx <- rgnX until rgnX + detail.rgnWidth) {
                        count(temp, xx, yy, detail)
                    }
                }

                //候補が洗い出せた！
                for (i <- 0 until size) {
                    if (-1 >= temp(i)) {
                        //候補
                        candidate.enqueue(i)
                    }
                }

                if (0 < candidate.size) {
                    Right(NumpreElementCandidate(x, y, candidate))
                } else {
                    //候補が一つも無ければ、この問題の解法を見つけることは出来ない。
                    Left(SolverImpossible)
                }
            } else {
                //既に値は埋まっている。
                Right(NumpreElementCandidate(x, y, Iterable[Int]()))
            }
        } else {
            //x,yが不正値
            Left(SolverInvalidParameter)
        }
    }

    def count(info: Array[Int], x: Int, y: Int, detail: NumpreDetail) : Boolean = {
        val v = detail.atValue(x, y)
        if (0 <= v) {
            info(v) = info(v) + 1
            true
        } else {
            false
        }
    }

    private def getRgn(x: Int, y: Int, detail: NumpreDetail) : (Int, Int) = {
        (x / detail.rgnWidth, y / detail.rgnHeight)
    }
}
