import collections
import threading
import numpy as np
from scipy.interpolate import pchip_interpolate
from scipy.optimize import minimize
from multiprocessing import Pool

CORPUS_SIZE = 2

sequences = dict()
counter = collections.Counter()
events = collections.defaultdict(threading.Event)
enqueue = None


def task(s, log_q_to_s):
    return s.pixels * s.bd_rate(log_q_to_s)


def run():
    quantizers = [78, 98, 118, 138, 158, 188]
    bounds = [1.5, 3.0]
    log_quantizers = [q_to_log(q) for q in quantizers]

    def wait_results(samples):
        keys = [events[k] for k in samples]
        new_samples = [k for k in samples if k not in counter]
        for k in new_samples:
            counter[k] = 0
        enqueue(new_samples)
        for k in keys:
            k.wait()

    def measure(Y, ihz=3 / 2**3):
        hz = 1.0 / ihz
        X = [log_quantizers[0], log_quantizers[-1]]
        log_q_to_s = np.poly1d(np.polyfit(X, Y, 1))
        samples = []
        for q, log_q in zip(quantizers, log_quantizers):
            s = log_q_to_s(log_q)
            ub = np.ceil(s * hz) * ihz
            samples.append((q, ub))
            lb = np.floor(s * hz) * ihz
            if lb != ub:
                samples.append((q, lb))
        wait_results(samples)
        return mean_bd_rate(log_q_to_s)

    def mean_bd_rate(log_q_to_s):
        total_pixels = sum(s.pixels for s in sequences.values())
        weighted_total = 0
        for partial_sum in pool.starmap(task, [(s, log_q_to_s)
                                               for s in sequences.values()]):
            weighted_total += partial_sum
        return weighted_total / total_pixels

    pool = Pool(16)
    samples = [(q, s) for q in quantizers for s in bounds]
    wait_results(samples)
    for sequence in sequences.values():
        sequence.bootstrap()
    options = {"disp": True}
    bounds = [(bounds[0], bounds[1])] * 2
    x0 = (2.5, 2.5)
    for exp in range(4, 21):
        args = (3.0 / 2**exp, )
        r = minimize(measure, x0, args=args, bounds=bounds, options=options)
        print(r.x.tolist())
        print(r)
        x0 = r.x
    enqueue([None])


def open(enqueue_fn):
    global enqueue
    enqueue = enqueue_fn
    threading.Thread(target=run).start()


def close():
    for name, sequence in sequences.items():
        print(name)
        print(sequence.__dict__)
    print("Terminating.")


def reduce_log(filename, fileobj):
    name = filename[:filename.index('.zst')]
    rate = 0
    metric = None
    for line in fileobj:
        if b'Input Frame' in line:
            rate += int(line.split(maxsplit=9)[-2])
        if b'Mean PSNR' in line:
            metric = float(line.split(maxsplit=5)[-2])
    return (name, rate, metric)


def progress(q, s, results):
    for name, rate, metric in results:
        if name not in sequences:
            sequences[name] = SequenceData(name)
        sequences[name].receive(q, s, rate, metric)
    counter[(q, s)] += len(results)
    if counter[(q, s)] == CORPUS_SIZE:
        events[(q, s)].set()


class SequenceData:

    def __init__(self, filename):
        detail = filename[:-len('.y4m')]
        detail, start, end = detail.split('-')
        _, detail, fps = detail.rsplit('_', 2)
        width, height = detail.split('x')
        self.width = int(width)
        self.height = int(height)
        self.start = int(start)
        self.end = int(end)
        self.pixels = self.width * self.height * (self.end - self.start)
        self.metric = dict()
        self.log_rate = dict()
        self.y_min = None
        self.y_max = None
        self.baseline = 0

    def bootstrap(self):
        quantizers = sorted(self.metric.keys())
        self.y_max = min(self.metric[quantizers[0]].values())
        self.y_min = max(self.metric[quantizers[-1]].values())
        self.baseline = self.bd_rate(lambda log_q: 2.625)

    def receive(self, quantizer, strength, rate, metric):
        if quantizer not in self.metric:
            self.metric[quantizer] = dict()
            self.log_rate[quantizer] = dict()
        self.metric[quantizer][strength] = metric
        self.log_rate[quantizer][strength] = np.log2(rate)

    def interp_metric(self, quantizer, strength):
        data = np.array(sorted(self.metric[quantizer].items()))
        return pchip_interpolate(*data.T, strength)

    def interp_log_rate(self, quantizer, strength):
        data = np.array(sorted(self.log_rate[quantizer].items()))
        return pchip_interpolate(*data.T, strength)

    def bd_rate(self, log_q_to_s):
        metric = []
        log_rate = []
        quantizers = sorted(self.metric.keys())
        for q in quantizers[::-1]:
            log_q = q_to_log(q)
            s = log_q_to_s(log_q)
            metric.append(self.interp_metric(q, s))
            log_rate.append(self.interp_log_rate(q, s))
        Y = np.linspace(self.y_min, self.y_max, 50)
        X = pchip_interpolate(metric, log_rate, Y)
        return np.trapz(X, Y) / (self.y_max - self.y_min) - self.baseline


q_to_log = lambda q: raw_log_q[q] / 2**57
raw_log_q = np.array([
    0, 0, 24488773541648927, 46394727941594200, 66211074127908984,
    84301980808752399, 100944001715702973, 116352106436299575,
    130696708750346599, 144115188075855872, 156719911892297261,
    168603961617504799, 179845308304957481, 190509916017450072,
    200654087245051973, 210326262203764856, 219568417842375269,
    228417168884608271, 236904643959044272, 245059189791558845,
    252905942426257198, 260467294512155447, 267763280554175909,
    274811896826202471, 281629368808548139, 288230376151711744,
    294628243012517256, 300835099968153133, 306862022453749647,
    312719149693360671, 318415787332448847, 323960496380813353,
    329361170600311244, 334625104093305944, 339759050544146163,
    344769275320907845, 349661601445713158, 354441450279620728,
    359113877634954870, 363683605918231141, 368155052816007390,
    372532356960464143, 376819400948455021, 381019832034900144,
    385137080776905532, 389174377867414717, 393134769365349692,
    397021130502113070, 400836178221214928, 404582482588011319,
    408262477189565752, 411878468630031781, 415432645214340435,
    418927084902058343, 422363762603808454, 425744556884404011,
    429071256129660245, 432345564227567616, 435569105809008917,
    438743431088373128, 441870020340170551, 444950288044009005,
    447985586726983541, 450977210529605519, 453926398518802537,
    456834337769216543, 459702166231977676, 462530975408304719,
    465321812843652544, 468075684456669225, 470793556715920303,
    473476358676167116, 476124983884935015, 478740292169161816,
    481323111310865469, 483874238620002035, 486394442411991713,
    488884463396763717, 491345015985603205, 494978619480176606,
    496180449438784181, 498556638355476600, 502067521408036484,
    503229065710810742, 506662639148900652, 507798793994087013,
    510046537693156410, 513370326645063344, 514470412398263425,
    516647545036320015, 519868352666398829, 520934589024310893,
    523045411897125526, 526169427984393247, 527203835858030478,
    529252268852761404, 533289565943270589, 538226087933006271,
    541136318577968942, 544951366297070800, 549621740157879954,
    552377665265421624, 555993656705887653, 559547833290196307,
    563042272977914215, 567327585054209380, 570694746506305050,
    574008244007486534, 577269761316872458, 580480904170458772,
    582068066810140878, 585985208416026423, 589065476119864877,
    592100774802839413, 595092398605461391, 598041586594658409,
    600949525845072415, 603817354307833548, 606646163484160591,
    609437000919508416, 612190872532525097, 616923009826938535,
    620240171960790887, 624150900430386588, 627989426695857907,
    631133999533341160, 634846885860216319, 639696459293503006,
    643260820265140590, 646765103912297405, 649641672379570163,
    653040891425754792, 655830989607746498, 660219874784196168,
    663451789538979912, 667157948242049657, 668729118706701853,
    671832393538036789, 676903754815359137, 680382097505306559,
    683802056019840975, 688592944744756310, 691882592010372350,
    695119970365458229, 698759717806032830, 703661154839428310,
    706723401410317546, 709740280195473740, 713134874248228070,
    716059935216565254, 720574354107204242, 724198550142056557,
    727759873458655637, 731260493397445632, 735838622596608079,
    739207586681317263, 742522177976692988, 747218057553513573,
    750409550433831421, 753552188995364288, 756647463652747684,
    761705666886685274, 765012277698947029, 768588436465532409,
    773053035292848890, 776495124433476493, 779880699650669844,
    784409967174896231, 787670922392901654, 790881098604620295,
    794326490009849316, 799113405805083523, 802426856550689554,
    807031801914717786, 810486571655702163, 813624964770797828,
    816971798111258124, 821769539174253209, 825234862089070621,
    830086124130885405, 833417740583227054, 836695740275161088,
    840151086248078554, 845117810426595446, 848876428668484264,
    852352614038749183, 855770770151427876, 861002238707389713,
    864487093952424822, 868313738217912429, 871875061534511509,
    875568730987603254, 879387285381700871, 883136971073078441,
    887002449391680598, 890797225904868214, 894700193629103573,
    898704072205362360, 902632149955715688, 906653182335707366,
    910923296342374256, 915106984955675290, 919207695725361300,
    923686983018285536, 925667178767253025, 930014099572075497,
    934416127407979396, 939150903650868320, 941128408754110064,
    945718619275032844, 950341634611982653, 955124138397510230,
    957089360126491165, 961979327576629927, 967000556927854914,
    968980752676822402, 974081752522974087, 976107342418585273,
    981275023468122190, 986536015550391703, 988561218563261525,
    993865783566538846, 995815036929476511, 1001365250399939814,
    1006862739396859828, 1008805919965241517, 1014411752313570492,
    1016380696275723326, 1022266571624913439, 1024255789316102989,
    1030018760946328609, 1032025013509402950, 1038021395279288313,
    1040040043824542905, 1046151530227318185, 1048178378513356554,
    1054302137947429348, 1056333408777009416, 1062614886965400099,
    1064647196405601121, 1066640535849433831, 1068670866284078951,
    1070662301857208854, 1072687974675497120, 1074674929182525704,
    1076693577727780296, 1078673773476747784, 1080683316724419108,
    1082654750018942221, 1084653367323019771, 1086676927731705876,
    1088661849771669014, 1090669863859998889, 1092698942363649961,
    1094689173499830127, 1096698901748736625
])
