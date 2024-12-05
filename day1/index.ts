const file = await Bun.file("input.txt").text();

const [list1, list2] = file
  .split("\n")
  .filter((line) => line.length > 0)
  .map((line) => line.split("   "))
  .reduce(
    (acc, val) => {
      const [val1, val2] = val;

      acc[0].push(parseInt(val1));
      acc[1].push(parseInt(val2));
      return acc;
    },
    [[], []] as [number[], number[]],
  );

list1.sort((a, b) => a - b);
list2.sort((a, b) => a - b);

const result1 = list1.reduce(
  (acc, val, index) => acc + Math.abs(val - list2[index]),
  0,
);

console.log(result1);

const lookup = {} as Record<number, number>;

list2.forEach((val) => {
  lookup[val] = (lookup[val] || 0) + 1;
});

const result2 = list1.reduce((acc, val) => acc + (val * lookup[val] || 0), 0);
console.log(result2);
