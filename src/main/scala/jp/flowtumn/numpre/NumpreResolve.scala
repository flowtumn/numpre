package jp.flowtumn.numpre

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/**
 * 結果。
 */
trait SolverResult

/**
 * 解法成功。
 */
object SolverSuccess extends SolverResult

/**
 * まだ見つかっていない。
 */
object SolverNotFound extends SolverResult

/**
 * 不可能。
 */
object SolverImpossible extends SolverResult

/**
 * パラメーターが不正。
 */
object SolverInvalidParameter extends SolverResult

/**
 * 失敗したが理由が不詳。
 */
object SolverFailUnknown extends SolverResult

/**
 * 解法スキャンの実行を持つtrait.
 */
trait NumpreStrategy {
	def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, Option[NumpreElementCandidate]]
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
 * Numpreの詳細の情報を持っているtrait.
 */
trait NumpreDetail extends NumpreInfo {
	/**
	 * 指定マスを消去。
	 */
	def erase(x: Int, y: Int): Unit

	/**
	 * 指定マスに値を設定。
	 */
	def putValue(numpreElement: NumpreElement): Unit

	/**
	 * そのマスの値を取得。
	 */
	def atValue(x: Int, y: Int): Int

	/**
	 * 現在の状態を取得します。
	 */
	def getResult(strategy: NumpreStrategy): SolverResult = {
		scanInternal(0, 0, strategy)
	}

	@tailrec
	private def scanInternal(x: Int, y: Int, strategy: NumpreStrategy, workTemp: Array[Int] = Array.fill(this.width)(0)): SolverResult = {
		val size = this.width

		//次の座標へ
		val f = (xx: Int, yy: Int, max: Int) => {
			if ((xx == max) && (yy == max)) {
				(xx, yy)
			} else {
				if (xx + 1 >= max) {
					(0, yy + 1)
				} else {
					(xx + 1, yy)
				}
			}
		}

		val (xx, yy) = f(x, y, size)

		strategy.scan(x, y, this) match {
			case Right(Some(v)) =>
				//候補になった値は控えておく
				v.values.foreach(each => workTemp(each) = workTemp(each) + 1)
			case Right(e) =>
				//候補が見つからない、今の値でカウント
				workTemp(atValue(x, y)) = workTemp(atValue(x, y)) + 1
			case Left(r) =>
				//解法を得ることは不可能
				return r
		}

		if ((xx >= size) || (yy >= size)) {
			if (size == workTemp.filter(_ == size).toList.size) {
				//解法済み。
				SolverSuccess
			} else {
				//まだ解法は見つかっていない。
				SolverNotFound
			}
		} else {
			scanInternal(xx, yy, strategy, workTemp)
		}
	}
}

class NumpreResolve(private val info: NumpreInfo, val initData: Iterable[NumpreElement]) extends NumpreStrategy {
	private val repository = new AtomicReference[NumpreRepository](OnMemoryRepositoryFactory.create(initData))
	private val table = Array.fill[Int](info.width, info.height)(-1)

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

		val detail = new NumpreDetail {
			override def width: Int = info.width
			override def height: Int = info.height
			override def rgnWidth: Int = info.rgnWidth
			override def rgnHeight: Int = info.rgnHeight
			override def diagonal: Boolean = info.diagonal
			override def erase(x: Int, y: Int): Unit = {
				table(x)(y) = -1
			}

			override def putValue(numpreElement: NumpreElement): Unit = {
				table(numpreElement.x)(numpreElement.y) = numpreElement.value
			}

			override def atValue(x: Int, y: Int): Int = {
				table(x)(y)
			}
		}

		val datas = scanAll(detail)

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

		s(detail, repository.get)
		println(detail.getResult(this))
		Right(SolverSuccess)
	}

	def s(detail: NumpreDetail, repository: NumpreRepository): Either[Exception, SolverResult] = {
		detail.getResult(this) match {
			case SolverSuccess => {
				Right(SolverSuccess)
			}
			case SolverImpossible => {
				//不可能。repositoryを元に消去する。
				repository.pop match {
					case Some(v) => {
						detail.erase(v.x, v.y)
						s(detail, repository)
					}
					case None => {
						Left(new Exception("none."))
					}
				}
			}
			case SolverNotFound => {
				val candidates = scanAll(detail).toList.sortWith((x, y) => x.values.size < y.values.size).filter(0 < _.values.size)

				//todo: 候補値が一個しか無い場合、即埋めてしまった方が早い。

				createNextNumpreElement(candidates, repository) match {
					case Some(v) =>
						detail.putValue(v)
						//repositoryに記録する。
						repository.push(v)
						//再帰
						s(detail, repository)
					case None =>
						Left(new Exception("None"))
				}
			}
			case _ => {
				Right(SolverFailUnknown)
			}
		}
	}

	/**
	 * Repositoryと候補を照らし合わせて、次の要素を作成する。
	 */
	def createNextNumpreElement(candidate: List[NumpreElementCandidate], repository: NumpreRepository): Option[NumpreElement] = {
		candidate.foreach(
			each => {
				each.values.foreach(
					value => {
						val r = NumpreElement(x = each.x, y = each.y, value = value)
						if (false == repository.isPushd(r)) {
							return Some(r)
						}
					}
				)
			}
		)
		None
	}

	def solverInternal(detail: NumpreDetail, repository: NumpreRepository): Either[Exception, SolverResult] = {
		val datas = scanAll(detail)

		val candidate = datas.toList.sortWith((x, y) => x.values.size < y.values.size).filter(0 < _.values.size)
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
					case Right(Some(v)) =>
						result.enqueue(v)
					case Right(e) =>
						result.enqueue(new NumpreElementCandidate(x = x, y = y, values = Vector[Int]()))
					case Left(e) =>
						// Noneなら候補は無い。(このループでNoneが返却されるのはDetailの実装ミス)
						result.enqueue(new NumpreElementCandidate(x = x, y = y, values = Vector[Int]()))
				}
			}
		}

		result
	}

	/**
	 * 指定した座標から候補になる値全てを列挙する。更に解法が見つけられるのかも判断する。
	 */
	def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, Option[NumpreElementCandidate]] = {
		val size = detail.height
		val candidate = new scala.collection.mutable.Queue[Int]
		val temp: Array[Int] = Array.fill(size)(-1);

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
					if (((x % size) + (y % size)) == size - 1) {
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

				//スキャンした結果、sizeを超えた分カウントされているのはありえないこと。
				if (temp.filter(_ >= size).isEmpty) {
					//候補が洗い出せた！
					for (i <- 0 until size) {
						if (-1 >= temp(i)) {
							//候補
							candidate.enqueue(i)
						}
					}

					if (0 < candidate.size) {
						Right(Some(NumpreElementCandidate(x, y, candidate.toVector)))
					} else {
						//候補が一つも無ければ、この問題の解法を見つけることは出来ない。
						Left(SolverImpossible)
					}
				} else {
					//スキャンした結果が不思議なことに。見つけるのは不可能。
					Left(SolverImpossible)
				}
			} else {
				//既に値は埋まっている。
				Right(None)
			}
		} else {
			//x,yが不正値
			Left(SolverInvalidParameter)
		}
	}

	/**
	 * X, Yの座標をカウントアップ。
	 */
	def count(refArray: Array[Int], x: Int, y: Int, detail: NumpreDetail): Boolean = {
		val v = detail.atValue(x, y)
		if (0 <= v) {
			refArray(v) = refArray(v) + 1
			true
		} else {
			false
		}
	}

	private def getRgn(x: Int, y: Int, detail: NumpreDetail): (Int, Int) = {
		(x / detail.rgnWidth, y / detail.rgnHeight)
	}
}
