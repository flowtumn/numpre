import jp.flowtumn.numpre._

object HelloWorld {
	def main(args: Array[String]) {

		//val init = "123\n000456\n789\n00304050\n295421\n\n\n321\n00000879"
		val init =
			"123456789"

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

			}
			case Left(e) => {
				println(e)
			}
		}

		val v = ((v1: Int, v2: Int) => {
			v1 * v2
		})(100000, 1200)
		println(v)
	}

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
}

