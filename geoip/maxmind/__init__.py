import copy 
import re


valid_ipv4_regex = re.compile(r"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")

def parse_network(network):
    mask, bitrange = network.split("/")
    return get_range(ipv4_to_int(mask), int(bitrange))

def get_range(mask, bitrange):
    return (mask, mask + (0xffffffff >> bitrange))

def valid_ipv4(addr):
    return bool(valid_ipv4_regex.match(addr))


def int_to_ipv4(i):
    return ".".join(["%i" % ((i & (0xff000000 >> x)) >> (24 - x)) for x in range(0, 32, 8)])


def ipv4_to_int(mask):
    return reduce(lambda total, byte: (total << 8) + byte,
                  map(int, mask.split(".")), 0)


class IPRangeLookup(object):
    def __init__(self, ranges):
        self.ranges = ranges

    def lookup(self, search_ip):
        lo = 0
        hi = len(self.ranges)
        while lo < hi:
            mid = (lo + hi) // 2
            if self.ranges[mid].in_range(search_ip):
                return self.ranges[mid]
            elif self.ranges[mid].start < search_ip:
                lo = mid + 1
            else:
                hi = mid


class IPRange(object):
    __slots__ = ["start", "end"]

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def in_range(self, ip):
        return self.start <= ip <= self.end


