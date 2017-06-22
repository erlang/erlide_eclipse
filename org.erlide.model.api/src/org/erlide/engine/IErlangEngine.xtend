package org.erlide.engine

import org.erlide.engine.model.root.IErlModel
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

    def boolean isAvailable()

// MODEL
    def IErlModel getModel()

    def ModelUtilService getModelUtilService()

// LANGUAGE services
    def CompletionService getCompletionService()

    def CleanupProvider getCleanupProvider() // codeLens?

    def SimpleScannerService getSimpleScannerService()

    def SearchServerService getSearchServerService()

    def ModelFindService getModelFindService()

// client!
    def ToggleCommentService getToggleCommentService()

    def IndentService getIndentService()

// client!
    def OpenService getOpenService()

    def OtpDocService getOtpDocService()

    def EdocExportService getEdocExportService()

// RUNTIME services
    def SystemInfoService getSystemInfoService()

    def ProclistService getProclistService()

// INTERNAL services (shouldn't really be exposed here)
    def String getStateDir()

    def ScannerProviderService getScannerProviderService()

    def SimpleParserService getSimpleParserService()

}
