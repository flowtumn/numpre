package jp.flowtumn.numpre

/**
 * 個々のマスの情報を表すclass.
 */
case class NumpreElement(val x: Int, val y: Int, val value: Int)

/**
 * 個々のマスで答えになる候補を表すclass.
 */
case class NumpreElementCandidate(val x: Int, val y: Int, val values: Iterable[Int] )

