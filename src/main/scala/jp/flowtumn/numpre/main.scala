import jp.flowtumn.numpre._

object HelloWorld {
	def str2NumpreData(data: String): Iterable[NumpreElement] = {
		val result = scala.collection.mutable.Stack[NumpreElement]()
		var x = 0
		var y = 0

		data.foreach(
			each => {
				if (each.equals('\n')) {
					x = 0
					y = y + 1
				} else {
					if ('0' <= each && '9' >= each) {
						result.push(NumpreElement(x = x, y = y, value = each.toInt - '0'.toInt - 1))
						x = x + 1
					}
				}
			}
		)
		result
	}

	def main(args: Array[String]) {
		val init: String = "7483921"
		new NumpreResolve(
			info = new NumpreInfo {
				override def width: Int = 9

				override def height: Int = 9

				override def rgnWidth: Int = 3

				override def rgnHeight: Int = 3

				override def diagonal: Boolean = true
			},
			initData = str2NumpreData(init)).solver match {
			case Right(v) => {
				v match {
					case SolverSuccess =>
						println("Success")
				}
			}
			case Left(e) => {
				println(e)
			}
		}
	}
}

