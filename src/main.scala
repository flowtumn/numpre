import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

/**
 * 個々のマスの情報を表すclass.
 */
class NumpreElement(val x: Int, val y: Int, val value: Int) {
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
     * 対角線。
     */
    def diagonal: Boolean
}

/**
 * 解法の内容を記しRepository.
 */
trait NumpreRepository {
    def push(v: NumpreElement): Boolean
    def pop: Option[NumpreElement]
    def toIterable: Iterable[NumpreElement]
}

/**
 *メモリ上に記憶するRepositoryを生成.
 */
object OnMemoryRepositoryFactory {
    def create(v: Iterable[NumpreElement]): NumpreRepository = {
        val result = new NumpreRepository {
            val collect = new scala.collection.mutable.Stack[NumpreElement]()
            
            override def push(v: NumpreElement): Boolean  ={
                //todo: 同一データは排除すべき。
                collect.push(v)
                true
            }

            override def pop: Option[NumpreElement] = {
                if (0 < collect.size) {
                    Some(collect.pop)
                } else {
                    None
                }
            }

            override def toIterable: Iterable[NumpreElement] = {
                collect
            }
        }

        v.foreach(result.push)
        result
    }
}

trait SolverResult
case class SolverSuccess extends SolverResult
case class SolverFail extends SolverResult

class NumpreResolver(private val info: NumpreInfo, val initData: Iterable[NumpreElement]) {
    private val repository = new AtomicReference[NumpreRepository](OnMemoryRepositoryFactory.create(initData))
    private val table = Array.ofDim[Int](info.width, info.height)

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

        Right(new SolverSuccess)
    }

    private def scan: Unit = {
    }
    
    private def solverInternal: Unit = {
    }
}

object HelloWorld {
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
                        result.push(new NumpreElement(x = x, y = y, value = each.toInt - '0'.toInt))
                        x = x + 1
                    }
                }
            }
        )
        result
    }
   def main(args: Array[String]) {
    
        val init = "123\n000456\n789\n00304050\n295421\n\n\n321\n00000879"
        
        new NumpreResolver(
            info = new NumpreInfo {
                def width: Int = 9
                def height: Int = 9
                def diagonal: Boolean = true
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
      })(100, 1200)
      println(v)
   }
}

