import jp.flowtumn.numpre._

object HelloWorld {


	def test: Unit = {
		//		val init: String = "123456789"
		val init: String = "800000000\n003600000\n070090200\n050007000\n000045700\n000100030\n001000068\n008500010\n090000400"
		//		val init:String = "123456789\n456789123\n789123456\n231674895\n875912364\n694538217\n317265948\n542897631\n960040500"

		val v = new NumpreResolve(
			info = new NumpreInfo {
				override def width: Int = 9

				override def height: Int = 9

				override def rgnWidth: Int = 3

				override def rgnHeight: Int = 3

				override def diagonal: Boolean = true
			},
			initData = str2NumpreData(init))

		val rr = v.atValue(8, 8)
		v.getResult match {
			case SolverSuccess =>
				val a = 0x00
			case _ =>
				val b = 0x00
		}
		v.scan(4, 4) match {
			case Right(None) => {
				assert(true)
			}
			case _ => {
				assert(false)
			}
		}

		v.scan(6, 1) match {
			case Right(Some(r)) => {
				assert(r.x == 6 && r.y == 1)
				val rr = List(1, 2, 3, 4, 5, 6)

				assert(rr.size == r.values.size)
				r.values.foreach(value => print((value + 1) + "  "))
				println
				assert(true)
			}
			case _ => {
				assert(false)
			}
		}
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

	def main(args: Array[String]) {

		//		test

		//val init = "123\n000456\n789\n00304050\n295421\n\n\n321\n00000879"
		val init = "987604321"
		//		val init: String = "800000000\n003600000\n070090200\n050007000\n000045700\n000100030\n001000068\n008500010\n090000400"
		new NumpreResolve(
			info = new NumpreInfo {
				override def width: Int = 9

				override def height: Int = 9

				override def rgnWidth: Int = 3

				override def rgnHeight: Int = 3

				override def diagonal: Boolean = false
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
}

