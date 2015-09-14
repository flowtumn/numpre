package jp.flowtumn.numpre

/**
 * 個々のマスの情報を表すclass.
 * 0からカウントしているため、0から扱うこと。
 */
case class NumpreElement(val x: Int, val y: Int, val value: Int) {
	def hash: Long = {
		(x << 24) | (y << 16) | value
	}
}

/**
 * 個々のマスで答えになる候補を表すclass.
 */
case class NumpreElementCandidate(val x: Int, val y: Int, val values: Vector[Int])

