package org.erlide.engine.model.builder;

import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.model.builder.BuilderTool;

/**
 * TODO there should be specific properties for each builder tool
 * but we wait until we see if there are many properties that are only
 * relevant for a single tool
 */
@SuppressWarnings("all")
public class BuilderProperties {
  /**
   * The tool that is used to build the project.
   */
  private BuilderTool _builderTool = BuilderTool.INTERNAL;
  
  /**
   * The tool that is used to build the project.
   */
  public BuilderTool getBuilderTool() {
    return this._builderTool;
  }
  
  /**
   * The tool that is used to build the project.
   */
  public void setBuilderTool(final BuilderTool builderTool) {
    this._builderTool = builderTool;
  }
  
  /**
   * Full build target name.
   */
  private String _buildTarget = "rebuild";
  
  /**
   * Full build target name.
   */
  public String getBuildTarget() {
    return this._buildTarget;
  }
  
  /**
   * Full build target name.
   */
  public void setBuildTarget(final String buildTarget) {
    this._buildTarget = buildTarget;
  }
  
  /**
   * Incremental build target name.
   */
  private String _compileTarget = "compile";
  
  /**
   * Incremental build target name.
   */
  public String getCompileTarget() {
    return this._compileTarget;
  }
  
  /**
   * Incremental build target name.
   */
  public void setCompileTarget(final String compileTarget) {
    this._compileTarget = compileTarget;
  }
  
  /**
   * Clean target name.
   */
  private String _cleanTarget = "clean";
  
  /**
   * Clean target name.
   */
  public String getCleanTarget() {
    return this._cleanTarget;
  }
  
  /**
   * Clean target name.
   */
  public void setCleanTarget(final String cleanTarget) {
    this._cleanTarget = cleanTarget;
  }
  
  /**
   * Test target name.
   */
  private String _testTarget = "test";
  
  /**
   * Test target name.
   */
  public String getTestTarget() {
    return this._testTarget;
  }
  
  /**
   * Test target name.
   */
  public void setTestTarget(final String testTarget) {
    this._testTarget = testTarget;
  }
  
  /**
   * Any extra flags required, as they would be specified on the command line.
   */
  private String _extraFlags = "";
  
  /**
   * Any extra flags required, as they would be specified on the command line.
   */
  public String getExtraFlags() {
    return this._extraFlags;
  }
  
  /**
   * Any extra flags required, as they would be specified on the command line.
   */
  public void setExtraFlags(final String extraFlags) {
    this._extraFlags = extraFlags;
  }
  
  public void fromString(final String data) {
    final String[] parts = data.split("|");
    try {
      String _get = parts[0];
      final BuilderTool bt = BuilderTool.valueOf(_get);
      final String b = parts[1];
      final String c = parts[2];
      final String l = parts[3];
      final String t = parts[4];
      this.setBuilderTool(bt);
      this.setBuildTarget(b);
      this.setCompileTarget(c);
      this.setCleanTarget(l);
      this.setTestTarget(t);
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
    BuilderTool _builderTool = this.getBuilderTool();
    _builder.append(_builderTool, "");
    _builder.append("|");
    String _compileTarget = this.getCompileTarget();
    _builder.append(_compileTarget, "");
    _builder.append("|");
    String _cleanTarget = this.getCleanTarget();
    _builder.append(_cleanTarget, "");
    _builder.append("|");
    String _testTarget = this.getTestTarget();
    _builder.append(_testTarget, "");
    _builder.append("|");
    String _extraFlags = this.getExtraFlags();
    _builder.append(_extraFlags, "");
    return _builder.toString();
  }
}
