package org.erlide.core.internal.builder

import com.google.common.base.Charsets
import com.google.common.io.Files
import java.net.URI
import org.eclipse.core.filesystem.EFS
import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.NullProgressMonitor

class ErlangToolExtensions {

    def private static hasTopFile(IContainer container, String filename) {
        return getTopFile(container, filename) !== null
    }

    def static getTopFile(IContainer container, String filename) {
        return container.members.findFirst[name == filename]
    }

    def static isUniversalMake(IFile makefile) {
        val file = getRealFile(makefile)
        if(file === null) return false

        val top = Files.readFirstLine(file, Charsets.ISO_8859_1)
        return top == "# Copyright 2012 Erlware, LLC. All Rights Reserved."
    }

    def static getRealFile(IResource ifile) {
        val URI uri = ifile.getRawLocationURI()
        if (uri === null)
            return null

        EFS.getStore(uri).toLocalFile(0, new NullProgressMonitor());
    }

    def static getMakefileTargets(IFile makefile) {
        val lines = Files.readLines(getRealFile(makefile), Charsets.ISO_8859_1)
        lines.map[if(hasTarget) split(":").head else null].filterNull
    }

    def private static hasTarget(String line) {
        line.matches("[a-z0-9_-]+:.*")
    }

    def static buildsWithMake(IProject project) {
        return project.hasTopFile("Makefile") && project.hasMakeBuilderEnabled
    }

    def static buildsWithEmake(IProject project) {
        return project.hasTopFile("Emakefile") && project.hasEmakeBuilderEnabled
    }

    def static buildsWithRebar(IProject project) {
        return project.hasTopFile("rebar.config") && project.hasRebarBuilderEnabled
    }

    def static hasMakeBuilderEnabled(IProject project) {
        return false;
    }

    def static hasEmakeBuilderEnabled(IProject project) {
        return false;
    }

    def static hasRebarBuilderEnabled(IProject project) {
        return false;
    }

}
