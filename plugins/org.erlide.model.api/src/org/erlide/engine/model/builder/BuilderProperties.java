package org.erlide.engine.model.builder;

import java.util.regex.Pattern;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;

/**
 * TODO there should be specific properties for each builder tool but we wait until we see
 * if there are many properties that are only relevant for a single tool
 */
@SuppressWarnings("all")
public class BuilderProperties {
    /**
     * The tool that is used to build the project.
     */
    private BuilderTool builderTool = BuilderTool.INTERNAL;

    /**
     * Full build target name.
     */
    private String buildTarget = "rebuild";

    /**
     * Incremental build target name.
     */
    private String compileTarget = "compile";

    /**
     * Clean target name.
     */
    private String cleanTarget = "clean";

    /**
     * Test target name.
     */
    private String testTarget = "test";

    /**
     * Any extra flags required, as they would be specified on the command line.
     */
    private String extraFlags = "";

    public void fromString(final String data) {
        final String[] parts = data.split(Pattern.quote("|"));
        try {
            final BuilderTool bt = BuilderTool.valueOf(parts[0]);
            final String b = parts[1];
            final String c = parts[1];
            final String l = parts[2];
            final String t = parts[3];
            builderTool = bt;
            buildTarget = b;
            compileTarget = c;
            cleanTarget = l;
            testTarget = t;
        } catch (final Throwable _t) {
            if (_t instanceof Exception) {
                return;
            } else {
                throw Exceptions.sneakyThrow(_t);
            }
        }
    }

    @Override
    public String toString() {
        final StringConcatenation _builder = new StringConcatenation();
        _builder.append(builderTool);
        _builder.append("|");
        _builder.append(compileTarget);
        _builder.append("|");
        _builder.append(cleanTarget);
        _builder.append("|");
        _builder.append(testTarget);
        _builder.append("|");
        _builder.append(extraFlags);
        return _builder.toString();
    }

    public BuilderTool getBuilderTool() {
        return builderTool;
    }

    public void setBuilderTool(final BuilderTool builderTool) {
        this.builderTool = builderTool;
    }

    public String getBuildTarget() {
        return buildTarget;
    }

    public void setBuildTarget(final String buildTarget) {
        this.buildTarget = buildTarget;
    }

    public String getCompileTarget() {
        return compileTarget;
    }

    public void setCompileTarget(final String compileTarget) {
        this.compileTarget = compileTarget;
    }

    public String getCleanTarget() {
        return cleanTarget;
    }

    public void setCleanTarget(final String cleanTarget) {
        this.cleanTarget = cleanTarget;
    }

    public String getTestTarget() {
        return testTarget;
    }

    public void setTestTarget(final String testTarget) {
        this.testTarget = testTarget;
    }

    public String getExtraFlags() {
        return extraFlags;
    }

    public void setExtraFlags(final String extraFlags) {
        this.extraFlags = extraFlags;
    }
}
