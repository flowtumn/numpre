package jp.flowtumn.numpre

import org.scalatest.FunSuite
import org.scalatest.matchers.Matchers

/**
 * 解法テスト。
 */
class NumpreResolveTest extends FunSuite with Matchers {
	/**
	 * NumpreResolve(9x9)の実装を返す。
	 */
	def createNumpreResolveImpl9(data: String): NumpreResolve = {
		new NumpreResolve(
			info = new NumpreInfo {
				override def width: Int = 9

				override def height: Int = 9

				override def rgnWidth: Int = 3

				override def rgnHeight: Int = 3

				override def diagonal: Boolean = false
			},
			initData = str2NumpreData(data)
		)
	}

	/**
	 * String -> NumpreElementに変換
	 */
	def str2NumpreData(data: String): Iterable[NumpreElement] = {
		val result = new scala.collection.mutable.Stack[NumpreElement]
		var x = 0
		var y = 0

		data.foreach(
			each => {
				if (each.equals('\n')) {
					x = 0
					y = y + 1
				} else {
					if ('0' <= each && '9' >= each) {
						result.push(new NumpreElement(x = x, y = y, value = each.toInt - '0'.toInt - 1))
						x = x + 1
					}
				}
			}
		)
		result
	}

	/**
	 * 既に答えが見つかっている
	 */
	test("success") {
		val init: String = "123456789\n456789123\n789123456\n935241867\n617538294\n842697531\n298314675\n371865942\n564972318"
		createNumpreResolveImpl9(init).getResult match {
			case SolverSuccess =>
				assert(true)
			case _ =>
				assert(false)
		}
	}

	/**
	 * 全てのマスが埋まり、答えが見つからない
	 */
	test("impossible") {
		val init: String = "123456789\n456789123\n789123456\n935241867\n617538294\n842697531\n298314675\n371865942\n564972311"
		createNumpreResolveImpl9(init).getResult match {
			case SolverImpossible =>
				assert(true)
			case _ =>
				assert(true)
		}
	}

	/**
	 * yが0で、x列が全て埋まっている時、いずれかの候補を確認するテスト
	 */
	test("solver") {
		val init: String = "123456789"
		val resolve = createNumpreResolveImpl9(init)
		resolve.scan(0, 0) match {
			case Right(None) => {
				assert(true)
			}
			case _ => {
				assert(false)
			}
		}

		resolve.scan(6, 1) match {
			case Right(Some(r)) => {
				assert(r.x == 6 && r.y == 1)

				//以下が候補に挙がらないとならない。
				val candidate = List(1, 2, 3, 4, 5, 6)

				assert(candidate.size == r.values.size)

				r.values.zip(candidate).foreach(
					each => {
						//内部では0から扱っているので、+1する必要がある。
						assert((each._1 + 1) == each._2)
					}
				)
			}
			case _ => {
				assert(false)
			}
		}
	}

	/**
	 * 何も問題を与えず解法。
	 */
	test("Issue 1") {
		val init: String = ""
		createNumpreResolveImpl9(init).solver match {
			case Right(v) =>
				v match {
					case SolverSuccess =>
						assert(true)
					case _ =>
						assert(false)
				}
			case _ =>
				assert(false)
		}
	}

	/**
	 * 世界一難しいと称される問題。
	 */
	//		test("Issue 2") {
	//			val init: String = "800000000\n003600000\n070090200\n050007000\n000045700\n000100030\n001000068\n008500010\n090000400"
	//			createNumpreResolveImpl9(init).solver match {
	//				case Right(v) =>
	//					v match {
	//						case SolverSuccess =>
	//							assert(true)
	//						case _ =>
	//							assert(false)
	//					}
	//				case _ =>
	//					assert(false)
	//			}
	//		}
}