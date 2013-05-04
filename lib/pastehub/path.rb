class PastehubPath
  def self.path
    libpath = File.dirname( __FILE__ )
    File.expand_path( libpath + "/../.." )
  end
end
