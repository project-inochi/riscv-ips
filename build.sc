import mill._, scalalib._
import mill.define.ModuleRef
import com.typesafe.config._
import java.io.File

val conf = ConfigFactory.parseFile(new File("version.conf")).resolve()
val spinalVersion = conf.getString("spinalVersion")

trait SpinalHDLModule extends SbtModule {
  override def forkArgs = Seq("-Xss16M")
  def scalaVersion = conf.getString("scalaVersion")

  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:$spinalVersion",
    ivy"com.github.spinalhdl::spinalhdl-lib:$spinalVersion"
  )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:$spinalVersion")
}

object hw extends SpinalHDLModule {
  override def millSourcePath = os.pwd / "hw"
  override def sources = T.sources(millSourcePath / "scala")
}

object test extends SpinalHDLModule {
  override def moduleDeps = Seq(hw)

  object test extends SbtTests with TestModule.ScalaTest {
    override def forkArgs = Seq("-Xss16M")
    override def millSourcePath = os.pwd / "test"
    override def sources = T.sources(millSourcePath / "scala")
    override def ivyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-tester::${spinalVersion}")
  }
}
