package org.erlide.engine.model.builder;

import org.erlide.engine.model.builder.BuilderTool;

@SuppressWarnings("all")
public class BuilderProperties {
  /**
   * TODO is this necessary?
   */
  private boolean _nukeOutputOnClean = false;
  
  /**
   * TODO is this necessary?
   */
  public boolean isNukeOutputOnClean() {
    return this._nukeOutputOnClean;
  }
  
  /**
   * TODO is this necessary?
   */
  public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
    this._nukeOutputOnClean = nukeOutputOnClean;
  }
  
  /**
   * The tool that is used to build the project.
   */
  private BuilderTool _builderTool;
  
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
  private String _buildTarget;
  
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
  private String _compileTarget;
  
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
  private String _cleanTarget;
  
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
}
