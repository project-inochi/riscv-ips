import mill._, scalalib._
import mill.define.ModuleRef
import com.typesafe.config._
import java.io.File

val conf = ConfigFactory.parseFile(new File("version.conf")).resolve()
val spinalVersion = conf.getString("spinalVersion")

trait SpinalHDLModule extends SbtModule {
  def scalaVersion = conf.getString("scalaVersion")

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion"
  )

  object test extends SbtTests with TestModule.ScalaTest {
    def ivyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-tester::${spinalVersion}")
  }

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion")
}

object hw extends SpinalHDLModule {
  override def millSourcePath = os.pwd / "hw"
  def sources = T.sources(
    millSourcePath / "spinal"
  )
}

object test extends SpinalHDLModule {
  override def millSourcePath = os.pwd / "test"

  def moduleDeps = Seq(hw)

  def sources = T.sources(
    millSourcePath / "spinal"
  )
}
