package org.erlide.engine.model.news

class ErlangElements {
    def static IErlangProject getProject(IErlangLibrary library) {
        library.parent as IErlangProject
    }
}
