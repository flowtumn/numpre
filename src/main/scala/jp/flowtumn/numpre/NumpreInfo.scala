package jp.flowtumn.numpre

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
 * Numpreの詳細の情報を持っているtrait.
 */
trait NumpreDetail extends NumpreInfo {
	/**
	 * 指定マスを消去。
	 */
	def erase(x: Int, y: Int): Unit

	/**
	 * Debug
	 */
	def dump: Unit

	/**
	 * 指定マスに値を設定。
	 */
	def putValue(numpreElement: NumpreElement): Unit

	/**
	 * そのマスの値を取得。
	 */
	def atValue(x: Int, y: Int): Int

	/**
	 * Strategyの実装を返します。
	 */
	def strategy: NumpreStrategy

	/**
	 * 現在の状態を取得します。
	 */
	def getResult: SolverResult = {
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
				//
			case Right(e) =>
				//候補が見つからない、今の値でカウント。
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
