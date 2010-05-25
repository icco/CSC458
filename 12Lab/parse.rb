#!/usr/bin/ruby

require 'rubygems'
require 'google_chart' # sudo gem install gchart

class Array
   def sum
      inject( nil ) { |sum,x|
         sum ? sum+x : x
      }
   end

   def mean
      (sum / size)
   end

   def std_dev
      n = 0
      mean = 0.0
      s = 0.0
      each { |x|
         n = n + 1
         delta = x - mean
         mean = mean + (delta / n)
         s = s + delta * (x - mean)
      }

      Math.sqrt(s/n)
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
   values.map! { |line|
      line = line.strip.match(/^(.*?,){4}(\d+\.\d+).*$/)[2].to_f
   }

   # calculate differences between days
   diffs = []
   for i in 1..(values.length-1) do
      diffs[i-1] = values[i] - values[i-1]
   end

   # take log of each, calculate std dev and mean
   diffs.map! { |val| if val.zero? then 0.0 else val = Math.log10 val.abs end }

   #stock = [ stock, diffs.std_dev, diffs.mean ]
   stock = [ diffs.std_dev, diffs.mean ]
}

# graph. risk vs return
stocks.each { |s| p s }

puts "\nChart Url"
sc = GoogleChart::ScatterChart.new('400x400',"Risk vs Return")
sc.data "Scatter Set", stocks
puts sc.to_url

