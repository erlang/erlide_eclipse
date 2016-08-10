package org.erlide.engine

import io.typefox.lsapi.services.LanguageServer
import org.erlide.engine.model.root.IErlModel
import org.erlide.engine.model.root.IErlModule
import org.erlide.engine.model.root.IErlProject
import org.erlide.engine.services.SystemInfoService
import org.erlide.engine.services.ToggleCommentService
import org.erlide.engine.services.cleanup.CleanupProvider
import org.erlide.engine.services.codeassist.CompletionService
import org.erlide.engine.services.edoc.EdocExportService
import org.erlide.engine.services.parsing.ScannerProviderService
import org.erlide.engine.services.parsing.SimpleParserService
import org.erlide.engine.services.parsing.SimpleScannerService
import org.erlide.engine.services.proclist.ProclistService
import org.erlide.engine.services.search.ModelFindService
import org.erlide.engine.services.search.ModelUtilService
import org.erlide.engine.services.search.OpenService
import org.erlide.engine.services.search.OtpDocService
import org.erlide.engine.services.search.SearchServerService
import org.erlide.engine.services.text.IndentService

interface IErlangEngine {

    def void initialize(ErlangInitializeParams params)

    def void shutdown()

    def LanguageServer getLanguageServer()

    def boolean isAvailable()

    def IErlModel getModel()

// LANGUAGE services

    def CompletionService getCompletionService(IErlProject project, IErlModule module, String elementBefore)

    def ModelUtilService getModelUtilService()

    def CleanupProvider getCleanupProvider()

    def ScannerProviderService getScannerProviderService()

    def SimpleScannerService getSimpleScannerService()

    def SimpleParserService getSimpleParserService()

    def SearchServerService getSearchServerService()

    def ModelFindService getModelFindService()

    def ToggleCommentService getToggleCommentService()

    def IndentService getIndentService()

    def OpenService getOpenService()

    def OtpDocService getOtpDocService()

    def EdocExportService getEdocExportService()

// RUNTIME services
    def SystemInfoService getSystemInfoService()

    def ProclistService getProclistService()

// INTERNAL services (shouldn't really be exposed here)
    def String getStateDir()

}
