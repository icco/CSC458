#!/usr/bin/ruby

# uncomment these and some lines below to enable google char
# google chart doesn't seem to handle the small differences this program
# outputs
#require 'rubygems'
#require 'google_chart' # sudo gem install gchart

class Array
   def return # Gemetric Average
      (inject(:*) ** (1.0/(size-1.0)))
   end

   def mean # Arithmetic mean
      inject(:+) / size.to_f
   end

   def std_dev
      m = mean; c = size
      Math.sqrt( inject(0) { |sum, e| sum + ((e - m) ** 2) } / c.to_f )
   end
end

# open each stock in stocks.txt
f = File.new "stocks.txt"
stocks = f.readlines.sort.map {|x| x.strip!}

# parse the lines, regex with /^(.*?,){4}(\d+\.\d+).*$/
stocks.map! { |stock|
   csv = File.new "stocks/#{stock}.csv"
   values = csv.readlines
   values.delete_at(0)
   values.reverse!
   values.map! { |line|
      line = line.strip.match(/^(.*?,){5}(\d+\.\d+).*$/)[2].to_f
   }

   # calculate differences between days
   diffs = []
   for i in 1..(1000) do
      diffs[i-1] = values[i] / values[i-1]
   end

   # take log of each, calculate std dev and mean
   #diffs.map! { |val| if val.zero? then 0.0 else val = Math.log10 val.abs end }

   #output results as a csv to stdout
   puts "#{stock}, #{diffs.std_dev}, #{diffs.return}"
   stock = [ diffs.std_dev, diffs.return ]
}

devs = []
rets = []

stocks.each { |stock|
   devs.push stock[0]
   rets.push stock[1]
}

puts "std_dev of std_dev: #{devs.std_dev}, mean of returns: #{rets.mean}"
puts "mean of std_dev: #{devs.mean}, std_dev of returns: #{rets.std_dev}"

