package org.erlide.core.internal.builder

import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IProject

class ErlangToolExtensions {
    
    def static hasRebarConfig(IProject project) {
        return false
    }

    def static hasMakefile(IProject project) {
        return false
    }

    def static hasConcreteMk(IProject project) {
        return false
    }

    def static hasErlangMk(IProject project) {
        return false
    }

    def static isUniversalMake(IFile makefile) {
        return false
    }

}
