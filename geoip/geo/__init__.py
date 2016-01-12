import copy 
import re


valid_ipv4_regex = re.compile(r"^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$")


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


class IPRangeASN(IPRange):
    __slots__ = ["owner", "asn"]

    def __init__(self, start, end, owner, asn):
        super(IPRangeASN, self).__init__(start, end)
        self.owner = owner
        self.asn = asn

    def as_dict(self):
        base = {"subnet": [int_to_ipv4(self.start), int_to_ipv4(self.end)]}
        if self.asn:
            base["asn"] = self.asn
        if self.owner:
            base["owner"] = self.owner
        return base
        


class IPRangeLocation(IPRange):
    __slots__ = ["location", "lat", "lon"]

    def __init__(self, rng, lat, lon):
        super(IPRangeLocation, self).__init__(*rng)
        self.lat = lat
        self.lon = lon
        self.location = None

    def as_dict(self):
        seed = {}
        ext = {}
        if self.lat and self.lon:
            seed = dict(latitude=self.lat, longitude=self.lon)
        if self.location:
            ext = self.location.as_dict()
        return dict(seed, **ext)


class Location(object):
    __slots__ = ["continent", "country", "r1", "r2", "city"]

    def __init__(self, continent_code, continent_name, country_code, country_name, r1_code, r1_name, r2_code, r2_name, city):
        self.continent = (continent_code, continent_name)
        self.country = (country_code, country_name)
        self.r1 = (r1_code, r1_name)
        self.r2 = (r2_code, r2_name)
        self.city = city

    def __repr__(self):
        return "Location <%s > %s > %s > %s > %s>" % (self.continent[1], self.country[0], self.r1[0], self.r2[0], self.city)

    def as_dict(self):
        return {"continent": self.continent,
                "country": self.country,
                "r1": self.r1,
                "r2": self.r2,
                "city": self.city}
