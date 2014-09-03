package org.erlide.engine.model.builder;

import java.util.regex.Pattern;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.engine.model.builder.BuilderTool;

/**
 * TODO there should be specific properties for each builder tool
 * but we wait until we see if there are many properties that are only
 * relevant for a single tool
 */
@Accessors
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
    String _quote = Pattern.quote("|");
    final String[] parts = data.split(_quote);
    try {
      String _get = parts[0];
      final BuilderTool bt = BuilderTool.valueOf(_get);
      final String b = parts[1];
      final String c = parts[2];
      final String l = parts[3];
      final String t = parts[4];
      this.builderTool = bt;
      this.buildTarget = b;
      this.compileTarget = c;
      this.cleanTarget = l;
      this.testTarget = t;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        return;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  public String toString() {
    StringConcatenation _builder = new StringConcatenation();
    _builder.append(this.builderTool, "");
    _builder.append("|");
    _builder.append(this.compileTarget, "");
    _builder.append("|");
    _builder.append(this.cleanTarget, "");
    _builder.append("|");
    _builder.append(this.testTarget, "");
    _builder.append("|");
    _builder.append(this.extraFlags, "");
    return _builder.toString();
  }
  
  @Pure
  public BuilderTool getBuilderTool() {
    return this.builderTool;
  }
  
  public void setBuilderTool(final BuilderTool builderTool) {
    this.builderTool = builderTool;
  }
  
  @Pure
  public String getBuildTarget() {
    return this.buildTarget;
  }
  
  public void setBuildTarget(final String buildTarget) {
    this.buildTarget = buildTarget;
  }
  
  @Pure
  public String getCompileTarget() {
    return this.compileTarget;
  }
  
  public void setCompileTarget(final String compileTarget) {
    this.compileTarget = compileTarget;
  }
  
  @Pure
  public String getCleanTarget() {
    return this.cleanTarget;
  }
  
  public void setCleanTarget(final String cleanTarget) {
    this.cleanTarget = cleanTarget;
  }
  
  @Pure
  public String getTestTarget() {
    return this.testTarget;
  }
  
  public void setTestTarget(final String testTarget) {
    this.testTarget = testTarget;
  }
  
  @Pure
  public String getExtraFlags() {
    return this.extraFlags;
  }
  
  public void setExtraFlags(final String extraFlags) {
    this.extraFlags = extraFlags;
  }
}
