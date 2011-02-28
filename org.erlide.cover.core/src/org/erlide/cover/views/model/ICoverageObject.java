package org.erlide.cover.views.model;

import java.io.Serializable;

public interface ICoverageObject extends ICoverageStats, IStatsTreeObject,
        Serializable {

    public ICoverageObject findChild(String name);

    public String getHtmlPath();

    public void setHtmlPath(final String htmlPath);

}
