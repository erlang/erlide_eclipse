require 'find'
require 'pathname'
require 'fileutils'

module PDE
  def PDE.extractSourceFeatureVersion(file)
    f = File.open(file, 'r')
    line = f.read
    m = /<feature[^>]* version="([^"]*)"/.match(line)
    return m[1] unless m.nil?
  end

  def PDE.versionRegexp
    "[0-9]+\.[0-9]+\.[0-9]+"
  end

  def PDE.unqualifiedVersion(ver)
    /(#{versionRegexp})\..*/.match(ver)[1]
  end

  def PDE.getBuildTimestamp(dir, name="org.erlide")
    Find.find(dir) do |path|
      m = /^#{name}_#{versionRegexp}\.([^.]*).*/.match(Pathname(path).basename.to_s)
      return m[1] unless m.nil?
    end
  end

  def PDE.getBuildVersion(dir, name="org.erlide")
    Find.find(dir) do |path|
      m = /^#{name}_(#{versionRegexp}).*/.match(Pathname(path).basename.to_s)
      return m[1] unless m.nil?
    end
  end

  def PDE.renameOldSite(dir)
    branch = /^([a-z]+)_/.match(dir.basename.to_s)
    return unless branch
    branch = branch[1]

    kind = p2_kind(branch)
    version = getBuildVersion(dir)
    ts = getBuildTimestamp(dir)
    dest = "#{version}_#{kind}#{ts}"

    root = Pathname(dir).dirname.to_s
    FileUtils.mv(dir, File.join(root, dest))
  end

  def PDE.renameOldSites(dir)
    Pathname(dir).children.map { |path|
      renameOldSite(path)
    }
  end

  def PDE.mirror_p2(source, destination)
    puts "Mirroring: #{source} => #{destination}"

    run_eclipse("#{tools_dir}/buckminster/", {
      "application"=>"org.eclipse.equinox.p2.metadata.repository.mirrorApplication",
      "source"=>"#{source}",
      "destination"=>"#{destination}"
    })
    run_eclipse("#{tools_dir}/buckminster/", {
      "application"=>"org.eclipse.equinox.p2.artifact.repository.mirrorApplication",
      "source"=>"#{source}",
      "destination"=>"#{destination}"
    })
  end

  def PDE.p2_add_composite(child, container, tools_dir="#{ENV['HOME']}/erlide_tools")
    puts "p2_add_composite? #{child} -- #{container}"
    relpath = Pathname(child).relative_path_from(Pathname(container)).to_s
    system "bash org.erlide.releng/comp-repo.sh #{container} --eclipse #{tools_dir}/buckminster/ add #{relpath}"
  end

  def PDE.p2_remove_composite(child, container, tools_dir="#{ENV['HOME']}/erlide_tools")
    puts "p2_remove_composite? #{child} -- #{container}"
    relpath = Pathname(child).relative_path_from(Pathname(container)).to_s
    system "bash org.erlide.releng/comp-repo.sh #{container} --eclipse #{tools_dir}/buckminster/ remove #{relpath}"
  end

end
