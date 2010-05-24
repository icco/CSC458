#!/usr/bin/ruby

# STD Dev 
# from http://warrenseen.com/blog/2006/03/13/how-to-calculate-standard-deviation/

def variance(population)
   n = 0
   mean = 0.0
   s = 0.0
   population.each { |x|
      n = n + 1
      delta = x - mean
      mean = mean + (delta / n)
      s = s + delta * (x - mean)
   }

   return s / n
end

def std_dev(population)
   Math.sqrt(variance(population))
end

class Array; def sum; inject( nil ) { |sum,x| sum ? sum+x : x }; end; end

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

   stock = [ stock, (std_dev diffs), (diffs.sum / diffs.length) ]
}

# graph. risk vs return
p stocks

