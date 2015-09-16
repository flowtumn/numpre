package jp.flowtumn.numpre

import java.util.concurrent.atomic.AtomicReference

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
 * Numpreの解法の実装class.
 */
class NumpreResolve(private val info: NumpreInfo, val initData: Iterable[NumpreElement]) extends NumpreStrategy with NumpreDetail {
	private val repository = new AtomicReference[NumpreRepository](OnMemoryRepositoryFactory.create(initData))
	private val table = Array.fill[Int](info.width, info.height)(-1)

	override def rgnWidth: Int = info.rgnWidth

	override def rgnHeight: Int = info.rgnHeight

	override def diagonal: Boolean = info.diagonal

	override def strategy: NumpreStrategy = this

	override def erase(x: Int, y: Int): Unit = {
		table(y)(x) = -1
	}

	override def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, Option[NumpreElementCandidate]] = NumpreResolve.scan(x, y, detail)

	/**
	 * デバッグ用出力。
	 */
	override def dump: Unit = {
		for (y <- 0 until height) {
			for (x <- 0 until width) {
				print(atValue(x, y) + 1 + " ")
			}
			println
		}
	}

	override def width: Int = info.width

	override def height: Int = info.height

	override def atValue(x: Int, y: Int): Int = {
		table(y)(x)
	}

	override def putValue(numpreElement: NumpreElement): Unit = {
		table(numpreElement.y)(numpreElement.x) = numpreElement.value
	}

	/**
	 * Repositoryを取得。
	 */
	def repo: NumpreRepository = repository.get

	// repositoryからtableを復元。
	repository.get.toIterable.foreach(
		each => {
			this.putValue(each)
		}
	)

	/**
	 * 解法を行います。
	 */
	def solver: Either[Exception, SolverResult] = {
		solverInternal(this, repo)
	}

	def solverInternal(detail: NumpreDetail, repository: NumpreRepository): Either[Exception, SolverResult] = {
		detail.getResult match {
			case SolverSuccess =>
				return Right(SolverSuccess)
			case SolverImpossible =>
				return Right(SolverImpossible)
			case SolverNotFound => {
				//候補を取得
				val base = NumpreResolve.scanAll(detail).toList.sortWith((x, y) => x.values.size < y.values.size).filter(0 < _.values.size).head

				base.values.foreach(
					each => {
						val element = NumpreElement(x = base.x, y = base.y, value = each)

						//repositoryに記録する。
						repository.push(element)
						detail.putValue(element)

						//再帰
						solverInternal(detail, repository) match {
							case Right(v) =>
								v match {
									case SolverSuccess =>
										return Right(SolverSuccess)
									case _ =>
										//repositoryから記録を消去
										repository.pop

										//該当箇所を消去
										detail.erase(base.x, base.y)
								}
							case Left(e) =>
								Left(e)
						}
					}
				)
			}
		}
		Right(SolverImpossible)
	}

	def scanAll(): Iterable[NumpreElementCandidate] = NumpreResolve.scanAll(this)

	def scan(x: Int, y: Int): Either[SolverResult, Option[NumpreElementCandidate]] = NumpreResolve.scan(x, y, this)

	def count(refArray: Array[Int], x: Int, y: Int): Boolean = NumpreResolve.count(refArray, x, y, this)

	def getRgn(x: Int, y: Int): (Int, Int) = NumpreResolve.getRgn(x, y, this)
}

/**
 * todo: 現状の課題が解決されたら、整理する必要あり。
 */
object NumpreResolve {
	/**
	 * 全マスの候補を取得。
	 */
	def scanAll(detail: NumpreDetail): Iterable[NumpreElementCandidate] = {
		val result = scala.collection.mutable.Queue[NumpreElementCandidate]()

		for (y <- 0 until detail.height) {
			for (x <- 0 until detail.width) {
				scan(x, y, detail) match {
					case Right(Some(v)) =>
						result.enqueue(v)
					case Right(e) =>
						result.enqueue(NumpreElementCandidate(x = x, y = y, values = Vector[Int]()))
					case Left(e) =>
						// Noneなら候補は無い。(このループでNoneが返却されるのはDetailの実装ミス)
						result.enqueue(NumpreElementCandidate(x = x, y = y, values = Vector[Int]()))
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
		val candidate = scala.collection.mutable.Queue[Int]()
		val temp: Array[Int] = Array.fill(size)(-1)

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

	/**
	 * リージョンの開始位置を返します。
	 */
	def getRgn(x: Int, y: Int, detail: NumpreDetail): (Int, Int) = {
		(x / detail.rgnWidth * detail.rgnWidth, y / detail.rgnHeight * detail.rgnHeight)
	}
}
