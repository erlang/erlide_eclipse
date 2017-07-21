package org.erlide.engine.model.news

import org.eclipse.xtend.lib.annotations.Data

interface IErlangElement  {
    def IErlangElement getParent()

    def Iterable<IErlangElement> getChildren()

    def IErlangElement getChild(String id)
}

interface IErlangModel extends IErlangElement {
	def Iterable<IErlangProject> getProjects()

	def IErlangProject getProject(String name)
}

interface IErlangProject extends IErlangApplication {
	val static String NATURE_ID = "org.erlide.core.erlnature"

	def String getOtpVersion()

	def IErlangLibrary getOtpLibrary()
}

interface IErlangApplication extends IErlangElement {
    def ErlangApplicationProperties getProperties()

    def Iterable<IErlangSourceFolder> getSourceFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getSourceFolder(String path)

    def Iterable<IErlangSourceFolder> getIncludeFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getIncludeFolder(String path)

    def IErlangEbinFolder getBinaryFolder()

    def Iterable<IErlangSourceFolder> getTestFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getTestFolder(String path)

    def IErlangLibrary getDependencies()
}

/**
 * From *.app
 */
@Data
class ErlangApplicationProperties {
    String version
}

interface IErlangLibrary extends IErlangElement {
    def Iterable<IErlangApplication> getApplications()
}

interface IErlangFolder extends IErlangElement {
    def Iterable<? extends IErlangUnit> getUnits()

    def IErlangUnit getUnit(String name)
}

interface IErlangSourceFolder extends IErlangFolder {
    def Iterable<? extends IErlangFolder> getFolders()

    def IErlangFolder getFolder(String name)

	def Iterable<? extends IErlangSource> getSources()

	def IErlangSource getSource(String name)
}

interface IErlangEbinFolder extends IErlangFolder {
	def Iterable<? extends IErlangBeam> getBeams()

	def IErlangBeam getBeam(String name)
}

interface IErlangUnit extends IErlangElement {
	def String getFileExtension()

	def Iterable<IErlangForm> getForms()
}

interface IErlangSource extends IErlangUnit, ISourceFile {
	def IErlangComment getHeaderComment()

	def Iterable<IErlangError> getErrors()
}

interface ISourceFile {
}

interface IErlangModule extends IErlangSource {
}

interface IErlangHeader extends IErlangSource {
}

interface IErlangBeam extends IErlangUnit {
    def ErlangBeamProperties getProperties()
}

@Data
class ErlangBeamProperties {
}

