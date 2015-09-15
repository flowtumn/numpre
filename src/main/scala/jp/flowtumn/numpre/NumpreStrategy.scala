package jp.flowtumn.numpre

/**
 * 解法スキャンの実行を持つtrait.
 */
trait NumpreStrategy {
	def scan(x: Int, y: Int, detail: NumpreDetail): Either[SolverResult, Option[NumpreElementCandidate]]
}
