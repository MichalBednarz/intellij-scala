package org.jetbrains.plugins.scala
package compiler

import com.intellij.notification.{Notification, NotificationType, Notifications}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.compiler.{CompileTask, CompilerManager}
import com.intellij.openapi.components.AbstractProjectComponent
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.CompilerModuleExtension
import com.intellij.openapi.ui.Messages
import org.jetbrains.plugins.scala.extensions._
import org.jetbrains.plugins.scala.project._
import org.jetbrains.plugins.scala.project.settings.ScalaCompilerConfiguration
import org.jetbrains.plugins.scala.statistics.{FeatureKey, Stats}

/**
 * Pavel Fatin
 */

class ServerMediator(project: Project) extends AbstractProjectComponent(project) {

  private def isScalaProject = project.hasScala
  private val settings = ScalaCompileServerSettings.getInstance

  private def checkSettings(): Boolean =
    if (isScalaProject) {
      if (!checkCompilationSettings()) false
      else true
    }
    else true

  private def checkCompileServerDotty(): Boolean =
    if (!settings.COMPILE_SERVER_ENABLED && project.hasDotty) {
      val title = "Enable Scala Compile Server"
      val content = s"<html><body>Dotty projects require Scala Compile Server<br> <a href=''>Configure</a></body></html>"
      Notifications.Bus.notify(new Notification("scala", title, content, NotificationType.ERROR, new CompileServerLauncher.ConfigureLinkListener(project)))
      false
    }
    else true

  private def startCompileServer(): Boolean = {
    if (settings.COMPILE_SERVER_ENABLED && isScalaProject) {
      invokeAndWait {
        CompileServerManager.configureWidget(project)
      }

      if (CompileServerLauncher.needRestart(project)) {
        CompileServerLauncher.instance.stop()
      }

      if (!CompileServerLauncher.instance.running) {
        invokeAndWait {
          CompileServerLauncher.instance.tryToStart(project)
        }
      }
    }

    true
  }

  private def addBeforeTask(compileTask: CompileTask): Unit =
    CompilerManager.getInstance(project).addBeforeTask(compileTask)

  addBeforeTask(_ => checkSettings())
  addBeforeTask(_ => checkCompileServerDotty())
  addBeforeTask(_ => startCompileServer())

  private def checkCompilationSettings(): Boolean = {
    def hasClashes(module: Module) = module.hasScala && {
      val extension = CompilerModuleExtension.getInstance(module)
      val production = extension.getCompilerOutputUrl
      val test = extension.getCompilerOutputUrlForTests
      production == test
    }
    val modulesWithClashes = ModuleManager.getInstance(project).getModules.toSeq.filter(hasClashes)

    var result = true

    if (modulesWithClashes.nonEmpty) {
      invokeAndWait {
        val choice =
          if (!ApplicationManager.getApplication.isUnitTestMode) {
            Messages.showYesNoDialog(project,
              "Production and test output paths are shared in: " + modulesWithClashes.map(_.getName).mkString(" "),
              "Shared compile output paths in Scala module(s)",
              "Split output path(s) automatically", "Cancel compilation", Messages.getErrorIcon)
          }
          else Messages.YES

        val splitAutomatically = choice == Messages.YES

        if (splitAutomatically) {
          inWriteAction {
            modulesWithClashes.map(_.modifiableModel)
              .foreach { model =>
              val extension = model.getModuleExtension(classOf[CompilerModuleExtension])

              val outputUrlParts = extension.getCompilerOutputUrl match {
                case null => Seq.empty
                case url => url.split("/").toSeq
              }
              val nameForTests = if (outputUrlParts.lastOption.contains("classes")) "test-classes" else "test"

              extension.inheritCompilerOutputPath(false)
              extension.setCompilerOutputPathForTests((outputUrlParts.dropRight(1) :+ nameForTests).mkString("/"))

              model.commit()
            }

            project.save()
          }
        }

        result = splitAutomatically
      }
    }

    result
  }

  override def getComponentName: String = getClass.getSimpleName
}
