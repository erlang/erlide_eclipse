package org.erlide.cover.views.model;

/**
 * 
 * Data model for statatistics viewer
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class StatsTreeModel {

    private static StatsTreeModel model;

    private ICoverageObject root;

    private StatsTreeModel() {
        initialize();
    }

    public static StatsTreeModel getInstance() {
        if (model == null) {
            model = new StatsTreeModel();
        }
        return model;
    }

    public ICoverageObject getRoot() {
        return root;
    }

    public void clear() {
        root.removeAllChildren();
        root.setLiniesCount(0);
        root.setCoverCount(0);

        ModuleSet.clear();
    }

    public void addTotal(final int allLines, final int coveredLines) {
        final int all = root.getLinesCount() + allLines;
        final int cov = root.getCoverCount() + coveredLines;
        root.setLiniesCount(all);
        root.setCoverCount(cov);
    }

    public void setIndex(final String path) {
        root.setHtmlPath(path);
    }
    
    public void setRootLabel(final String name) {
        root.setLabel(name);
    }
    
    private void initialize() {
        root = new StatsTreeObject("total", 0, 0, ObjectType.PROJECT);

    }

}
