#include <iostream>
#include <vector>
#include <vector>
#include <cmath>

constexpr int TEST = 50;

int binsearch(std::vector<int> stuff, int target)
{
	int lo = 0;
	int hi = stuff.size() - 1;
	while (lo <= hi) // While lo is a valid index
	{
		int mid = floor((lo + hi) / 2); // Middle index
		if (stuff[mid] < target)
		{
			lo = mid + 1;
		}
		else if (stuff[mid] > target)
		{
			hi = mid - 1;
		}
		else // target found
		{
			return mid;
		}
	}

	return -1; // Not found, return invalid index;
}

int main()
{
	std::vector<int> stuff;
	for (int i = 0; i < TEST; ++i)
	{
		stuff.push_back(i);
	}

	std::cout << binsearch(stuff, 25) << std::endl;
}