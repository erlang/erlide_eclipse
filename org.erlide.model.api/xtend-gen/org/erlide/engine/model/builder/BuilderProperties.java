package org.erlide.engine.model.builder;

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
}
