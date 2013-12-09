package org.erlide.engine.model.builder;

@SuppressWarnings("all")
public abstract class BuildProperties {
  private boolean _nukeOutputOnClean = false;
  
  public boolean isNukeOutputOnClean() {
    return this._nukeOutputOnClean;
  }
  
  public void setNukeOutputOnClean(final boolean nukeOutputOnClean) {
    this._nukeOutputOnClean = nukeOutputOnClean;
  }
}
