package org.erlide.core.erlang;

import org.erlide.core.ErlangPlugin;

public final class ErlangCoreOptions {
    // *************** Possible IDs for configurable options.
    // ********************

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_LOCAL_VARIABLE_ATTR = ErlangPlugin.PLUGIN_ID
            + ".compiler.debug.localVariable"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_LINE_NUMBER_ATTR = ErlangPlugin.PLUGIN_ID
            + ".compiler.debug.lineNumber"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_SOURCE_FILE_ATTR = ErlangPlugin.PLUGIN_ID
            + ".compiler.debug.sourceFile"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_CODEGEN_UNUSED_LOCAL = ErlangPlugin.PLUGIN_ID
            + ".compiler.codegen.unusedLocal"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_CODEGEN_TARGET_PLATFORM = ErlangPlugin.PLUGIN_ID
            + ".compiler.codegen.targetPlatform"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_CODEGEN_INLINE_JSR_BYTECODE = ErlangPlugin.PLUGIN_ID
            + ".compiler.codegen.inlineJsrBytecode"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_DOC_COMMENT_SUPPORT = ErlangPlugin.PLUGIN_ID
            + ".compiler.doc.comment.support"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_DEPRECATION = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.deprecation"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_UNUSED_LOCAL = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.unusedLocal"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_UNUSED_PARAMETER = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.unusedParameter"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_UNUSED_PRIVATE_FUNCTION = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.unusedPrivateFunction"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_LOCAL_VARIABLE_HIDING = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.localVariableHiding"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_INVALID_EDOC = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.invalidEdoc"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_INVALID_EDOC_TAGS = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.invalidEdocTags"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_MISSING_Edoc_TAGS = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.missingEdocTags"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_MISSING_Edoc_COMMENTS = ErlangPlugin.PLUGIN_ID
            + ".compiler.problem.missingEdocComments"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_PB_MAX_PER_UNIT = ErlangPlugin.PLUGIN_ID
            + ".compiler.maxProblemPerUnit"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_SOURCE = ErlangPlugin.PLUGIN_ID
            + ".compiler.source"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_COMPLIANCE = ErlangPlugin.PLUGIN_ID
            + ".compiler.compliance"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_PRIORITIES = ErlangPlugin.PLUGIN_ID
            + ".compiler.taskPriorities"; //$NON-NLS-1$

    /**
     * Possible configurable option value for COMPILER_TASK_PRIORITIES.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_PRIORITY_HIGH = "HIGH"; //$NON-NLS-1$

    /**
     * Possible configurable option value for COMPILER_TASK_PRIORITIES.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_PRIORITY_LOW = "LOW"; //$NON-NLS-1$

    /**
     * Possible configurable option value for COMPILER_TASK_PRIORITIES.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_PRIORITY_NORMAL = "NORMAL"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_TAGS = ErlangPlugin.PLUGIN_ID
            + ".compiler.taskTags"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPILER_TASK_CASE_SENSITIVE = ErlangPlugin.PLUGIN_ID
            + ".compiler.taskCaseSensitive"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ERLANG_BUILD_ORDER = ErlangPlugin.PLUGIN_ID
            + ".computeErlangBuildOrder"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ERLANG_BUILD_RESOURCE_COPY_FILTER = ErlangPlugin.PLUGIN_ID
            + ".builder.resourceCopyExclusionFilter"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ERLANG_BUILD_DUPLICATE_RESOURCE = ErlangPlugin.PLUGIN_ID
            + ".builder.duplicateResourceTask"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ERLANG_BUILD_CLEAN_OUTPUT_FOLDER = ErlangPlugin.PLUGIN_ID
            + ".builder.cleanOutputFolder"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_INCOMPLETE_CLASSPATH = ErlangPlugin.PLUGIN_ID
            + ".incompleteClasspath"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_CIRCULAR_CLASSPATH = ErlangPlugin.PLUGIN_ID
            + ".circularClasspath"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_INCOMPATIBLE_ERTS_LEVEL = ErlangPlugin.PLUGIN_ID
            + ".incompatibleERTSLevel"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ERLANG_BUILD_INVALID_CLASSPATH = ErlangPlugin.PLUGIN_ID
            + ".builder.invalidClasspath"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ENCODING = ErlangPlugin.PLUGIN_ID
            + ".encoding"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ENABLE_CLASSPATH_EXCLUSION_PATTERNS = ErlangPlugin.PLUGIN_ID
            + ".classpath.exclusionPatterns"; //$NON-NLS-1$

    /**
     * Possible configurable option ErlangPlugin.PLUGIN_ID.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CORE_ENABLE_CLASSPATH_MULTIPLE_OUTPUT_LOCATIONS = ErlangPlugin.PLUGIN_ID
            + ".classpath.multipleOutputLocations"; //$NON-NLS-1$

    /**
     * Default task tag
     * 
     */
    public static final String DEFAULT_TASK_TAGS = "TODO,FIXME,XXX"; //$NON-NLS-1$

    /**
     * Default task priority
     */
    public static final String DEFAULT_TASK_PRIORITIES = "NORMAL,HIGH,NORMAL"; //$NON-NLS-1$

    // *************** Possible values for configurable options.
    // ********************

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String GENERATE = "generate"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String DO_NOT_GENERATE = "do not generate"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String PRESERVE = "preserve"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String OPTIMIZE_OUT = "optimize out"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String VERSION_R9 = "R9"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String VERSION_R10 = "R10"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String ABORT = "abort"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String ERROR = "error"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String WARNING = "warning"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String IGNORE = "ignore"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPUTE = "compute"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String INSERT = "insert"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String DO_NOT_INSERT = "do not insert"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String PRESERVE_ONE = "preserve one"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String CLEAR_ALL = "clear all"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String NORMAL = "normal"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String COMPACT = "compact"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String TAB = "tab"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String SPACE = "space"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String ENABLED = "enabled"; //$NON-NLS-1$

    /**
     * Possible configurable option value.
     * 
     * @see #getDefaultOptions()
     */
    public static final String DISABLED = "disabled"; //$NON-NLS-1$

    private ErlangCoreOptions() {
    }
}
