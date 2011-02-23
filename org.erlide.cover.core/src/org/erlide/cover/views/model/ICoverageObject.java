package org.erlide.cover.views.model;

public interface ICoverageObject extends ICoverageStats, IStatsTreeObject {
    
    public ICoverageObject findChild(String name);
    
    public String getHtmlPath();
    
    public void setHtmlPath(final String htmlPath);

}
