package org.erlide.engine

import io.typefox.lsapi.services.LanguageServer
import org.erlide.engine.model.root.IBeamLocator
import org.erlide.engine.model.root.IErlModel
import org.erlide.engine.model.root.IErlModule
import org.erlide.engine.model.root.IErlProject
import org.erlide.engine.model.root.IProjectConfiguratorFactory
import org.erlide.engine.services.GenericService
import org.erlide.engine.services.SystemInfoService
import org.erlide.engine.services.cleanup.CleanupProvider
import org.erlide.engine.services.codeassist.CompletionService
import org.erlide.engine.services.codeassist.ContextAssistService
import org.erlide.engine.services.edoc.EdocExportService
import org.erlide.engine.services.parsing.ParserService
import org.erlide.engine.services.parsing.ScannerProviderService
import org.erlide.engine.services.parsing.SimpleParserService
import org.erlide.engine.services.parsing.SimpleScannerService
import org.erlide.engine.services.proclist.ProclistService
import org.erlide.engine.services.search.ModelFindService
import org.erlide.engine.services.search.ModelSearcherService
import org.erlide.engine.services.search.ModelUtilService
import org.erlide.engine.services.search.OpenService
import org.erlide.engine.services.search.OtpDocService
import org.erlide.engine.services.search.SearchServerService
import org.erlide.engine.services.search.XrefService
import org.erlide.engine.services.text.IndentService

interface IErlangEngine {

    def void initialize(ErlangInitializeParams params)

    def void shutdown()

    def LanguageServer getLanguageServer()

    def boolean isAvailable()

    def String getStateDir()

    def IErlModel getModel()

    def ContextAssistService getContextAssistService()

    def SearchServerService getSearchServerService()

    def ModelUtilService getModelUtilService()

    def CleanupProvider getCleanupProvider()

    def ParserService getParserService()

    def ScannerProviderService getScannerProviderService()

    def EdocExportService getEdocExportService()

    def ProclistService getProclistService()

    def SimpleScannerService getSimpleScannerService()

    def SimpleParserService getSimpleParserService()

    def ModelFindService getModelFindService()

    def ModelSearcherService getModelSearcherService()

    def CompletionService getCompletionService(IErlProject project, IErlModule module, String elementBefore)

    def GenericService getGenericService()

    def IBeamLocator getIBeamLocator()

    def IndentService getIndentService()

    def OpenService getOpenService()

    def OtpDocService getOtpDocService()

    def SystemInfoService getSystemInfoService()

    def XrefService getXrefService()

// INTERNAL use
    def IProjectConfiguratorFactory getProjectConfiguratorFactory()

}
