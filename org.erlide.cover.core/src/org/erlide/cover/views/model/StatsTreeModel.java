package org.erlide.cover.views.model;


/**
 * 
 * Data model for stats viewer
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 *
 */
public class StatsTreeModel {
	
	private static StatsTreeModel model;
	
	private StatsTreeObject root;
	
	private StatsTreeModel() {
		initialize();
	}
	
	public static StatsTreeModel getInstance() {
		if(model == null) {
			model = new StatsTreeModel();
		}
		return model;
	}
	
	public IStatsTreeObject getRoot() {
		return root;
	}
	
	public void clear() {
	    root.removeAllChildren();
	    root.setLiniesCount(0);
	    root.setCoverCount(0);
	    root.setPercentage(0.0);
	    
	    ModuleSet.clear();
	}
	
	public void addTotal(int allLines, int coveredLines) {
	    int all = root.getLinesCount() + allLines;
	    int cov = root.getCoverCount() + coveredLines;
	    root.setLiniesCount(all);
	    root.setCoverCount(cov);
	    root.setPercentage(cov/(double)all * 100);
	}
	
	public void setIndex(String path) {
	    root.setHtmlPath(path);
	}
	
	private void initialize() {
		System.out.println("Initialize Stats model");
		root = new StatsTreeObject("total", 0, 0, 0);
		
	}
	

}
