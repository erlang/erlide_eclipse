package org.erlide.engine.model.news

import org.eclipse.jdt.annotation.NonNull
import org.eclipse.xtend.lib.annotations.Data

interface IErlangElement  {
    def IErlangElement getParent()

    @NonNull
    def Iterable<IErlangElement> getChildren()

    def IErlangElement getChild(String id)
}

interface IErlangModel extends IErlangElement {
	@NonNull
	def Iterable<IErlangProject> getProjects()

	def IErlangProject getProject(String name)
}

interface IErlangProject extends IErlangApplication {
	val static String NATURE_ID = "org.erlide.core.erlnature"

    @NonNull
	def String getOtpVersion()

    @NonNull
	def IErlangLibrary getOtpLibrary()
}

interface IErlangApplication extends IErlangElement {
    @NonNull
    def ErlangApplicationProperties getProperties()

    @NonNull
    def Iterable<IErlangSourceFolder> getSourceFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getSourceFolder(String path)

    @NonNull
    def Iterable<IErlangSourceFolder> getIncludeFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getIncludeFolder(String path)

    @NonNull
    def IErlangEbinFolder getBinaryFolder()

    @NonNull
    def Iterable<IErlangSourceFolder> getTestFolders()

    /**
     * @param path relative to application container
     */
    def IErlangSourceFolder getTestFolder(String path)

    @NonNull
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
    @NonNull
    def Iterable<IErlangApplication> getApplications()
}

interface IErlangFolder extends IErlangElement {
    @NonNull
    def Iterable<? extends IErlangUnit> getUnits()

    def IErlangUnit getUnit(String name)
}

interface IErlangSourceFolder extends IErlangFolder {
    @NonNull
    def Iterable<? extends IErlangFolder> getFolders()

    def IErlangFolder getFolder(String name)

	@NonNull
	def Iterable<? extends IErlangSource> getSources()

	def IErlangSource getSource(String name)
}

interface IErlangEbinFolder extends IErlangFolder {
	@NonNull
	def Iterable<? extends IErlangBeam> getBeams()

	def IErlangBeam getBeam(String name)
}

interface IErlangUnit extends IErlangElement {
	def String getFileExtension()

	@NonNull
	def Iterable<IErlangForm> getForms()
}

interface IErlangSource extends IErlangUnit, ISourceFile {
	def IErlangComment getHeaderComment()

	@NonNull
	def Iterable<IErlangError> getErrors()
}

interface ISourceFile {
}

interface IErlangModule extends IErlangSource {
}

interface IErlangHeader extends IErlangSource {
}

interface IErlangBeam extends IErlangUnit {
    @NonNull
    def ErlangBeamProperties getProperties()
}

@Data
class ErlangBeamProperties {
}

