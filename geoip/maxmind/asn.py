from geo import IPRange, IPRangeLookup, int_to_ipv4
import re
import csv


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


class ASNLookup(IPRangeLookup):
    asn_owner_regex = re.compile("(?P<asn>AS[0-9]*) ?(?P<owner>.*)")

    def __init__(self, asn_filename):
        asns = self.parse_asns(asn_filename)
        super(ASNLookup, self).__init__(asns)
    
    def parse_asns(self, filename):
        with open(filename) as f:
            reader = csv.reader(f)
            reader.next()
            return map(self.line_to_asnrange, reader)

    def line_to_asnrange(self, line):
        start, end, asn_owner = line
        start = int(start)
        end = int(end)
        match = self.asn_owner_regex.match(asn_owner)
        asn, owner = match.groups()
        if not owner:
            owner = None
        return IPRangeASN(start, end, owner, asn)

            

