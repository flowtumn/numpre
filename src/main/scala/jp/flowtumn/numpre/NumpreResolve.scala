package jp.flowtumn.numpre

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

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
}

trait SolverResult
case class SolverSuccess extends SolverResult
case class SolverFail extends SolverResult


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
            
        Right(new SolverSuccess)
    }

    /**
     * 全マスの候補を取得。
     */
    def scanAll(detail: NumpreDetail): Iterable[NumpreElementCandidate] = {
        val result = new scala.collection.mutable.Queue[NumpreElementCandidate]

        for (x <- 0 until detail.width) {
            for (y <- 0 until detail.height) {
                scan(x, y, detail) match {
                    case Some(v) =>
                        result.enqueue(v)
                    case None =>
                        // Noneなら候補は無い。(このループでNoneが返却されるのはDetailの実装ミス)
                        result.enqueue(new NumpreElementCandidate(x = x, y = y, values = Iterable[Int]()))
                }
            }
        }

        result
    }
    
    def scan(x: Int, y: Int, detail: NumpreDetail): Option[NumpreElementCandidate] = {
        val size = detail.height
        val candidate = new scala.collection.mutable.Queue[Int]
        var temp: Array[Int] = Array.fill(size)(-1);

        if (0 <= x && x < size && 0 <= y && y < size) {
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
            }
            Some(NumpreElementCandidate(x, y, candidate))
        } else {
            //x,yが不正値
            None
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
    
    /**
     * X, Yマスの候補を取得。
     */

    @tailrec
    private def scanInternal(detail: NumpreDetail): Unit = {
        val size = detail.height
        
        //全体をスキャン。
        for (y <- 0 until size) {
            for (x <- 0 until size) {
                var temp = Array.ofDim[Int](size)
                if (0 == detail.atValue(x, y)) {
                    //縦ラインのスキャン
                    for (yy <- 0 until size) {
                        temp(detail.atValue(x, yy)) = temp(detail.atValue(yy, x)) + 1
                    }
                    //横ラインのスキャン
                    for (xx <- 0 until size) {
                        temp(detail.atValue(xx, y)) = temp(detail.atValue(y,xx)) + 1
                    }
                    //対角線も揃えるか？
                    if (detail.diagonal) {
                        //対角線も調べる。
                        if (y == x) {
                            for (i <- 0 until size) {
                                temp(detail.atValue(i, i)) = temp(detail.atValue(i, i)) + 1
                            }
                        }

                        //反対の対角線を調べる。
                        if ((x + y) == (size + 1)) {
                            for (i <- 0 until size) {
                                temp(detail.atValue(size - i, i))
                            }
                        }
                    }

                    //リージョン位置を取得。
                    val (rgnX, rgnY) = getRgn(x, y, detail)

                    //リージョンを調べる。
                    for (yy <- rgnY until rgnY + detail.rgnHeight) {
                        for (xx <- rgnX until rgnX + detail.rgnWidth) {
                            temp(detail.atValue(xx, yy)) = temp(detail.atValue(xx, yy)) + 1
                        }
                    }
                    
                    //候補が洗い出せた！
                    for (i <- 0 until size) {
                        if (0 == temp(i)) {
                            
                        }
                    }
                }
            }
        }
        scanInternal(detail)
    }
    
    private def getRgn(x: Int, y: Int, detail: NumpreDetail) : (Int, Int) = {
        (x / detail.rgnWidth, y / detail.rgnHeight)
    }
    
    private def solverInternal: Unit = {
    }
}
