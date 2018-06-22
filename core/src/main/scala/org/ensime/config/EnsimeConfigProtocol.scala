// Copyright: 2010 - 2017 https://github.com/ensime/ensime-server/graphs
// License: http://www.gnu.org/licenses/gpl-3.0.en.html
package org.ensime.config

import akka.event.slf4j.Logger

import scalaz.ioeffect.IO
import scalaz._, Scalaz._

import org.ensime.sexp._

import org.ensime.util.file._
import org.ensime.util.path._
import org.ensime.util.ensimefile._
import org.ensime.io.Canon.ops._

import org.ensime.api._
import SexpReader.ops._

object EnsimeConfigProtocol {
  private def log = Logger(this.getClass.getName)

  def parse(
    config: String
  ): IO[Throwable, Either[DeserializationException, EnsimeConfig]] =
    SexpParser(config).as[EnsimeConfig].traverse(validated)

  def validated(c: EnsimeConfig): IO[Throwable, EnsimeConfig] = {
    // scalaz.Validation would be a cleaner way to do this
    {
      import c._
      val files = (rootDir :: javaHome :: javaSources).map { _.file.toFile }
      (files ::: javaRunTime(c)).foreach { f =>
        require(f.exists, "" + f + " is required but does not exist")
      }
    }

    val updatedProjects = c.projects.traverse(validated)

    updatedProjects.map(p => c.copy(projects = p))
  }

  def javaRunTime(c: EnsimeConfig): List[File] =
    c.javaHome.file.toFile.tree.filter(_.getName == "rt.jar").toList

  /*
   We use the canonical form of files/directories to keep OS X happy
   when loading S-Expressions. But the canon may fail to resolve if
   the file/directory does not exist, so we force create all required
   directories and then re-canon them, which is - admittedly - a weird
   side-effect.
   */
  private[config] def validated(
    p: EnsimeProject
  ): IO[Throwable, EnsimeProject] = {
    (p.targets ++ p.sources).foreach { dir =>
      if (!dir.exists() && !dir.isJar) {
        log.warn(s"$dir does not exist, creating")
        dir.file.mkdirs()
      }
    }
    p.canon
  }
}
