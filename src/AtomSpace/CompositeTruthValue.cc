/**
* CompositeTruthValue.cc
*
* @author Welter Silva
*/
#include "CompositeTruthValue.h"
#include "type_codes.h"
#include "TLB.h"


//#define USE_SHARED_DEFAULT_TV

void CompositeTruthValue::init(const TruthValue& tv, VersionHandle vh) {
    primaryTV = NULL;
    setVersionedTV(tv, vh);
    if (primaryTV == NULL) {
#ifdef USE_SHARED_DEFAULT_TV        
        primaryTV = (TruthValue*) &(TruthValue::DEFAULT_TV());
#else
        primaryTV = TruthValue::DEFAULT_TV().clone();
#endif        
    }    
}

void CompositeTruthValue::clear() {
    //printf("CompositeTruthValue::clear() of TV = %p\n", this);
#ifdef USE_SHARED_DEFAULT_TV    
    if (primaryTV != &(TruthValue::DEFAULT_TV())) {
        delete primaryTV;
    }
#else
    //printf("CompositeTruthValue::clear() => deleting primaryTV = %p\n", primaryTV);
    delete primaryTV;
#endif    
    for (VersionedTruthValueMap::const_iterator itr = versionedTVs.begin();
          itr != versionedTVs.end(); itr++) {
        TruthValue* tv = itr->second;
#ifdef USE_SHARED_DEFAULT_TV    
        if (tv != &(TruthValue::DEFAULT_TV())) {
            delete tv;
        }
#else
        //printf("CompositeTruthValue::clear() => deleting versioned tv  = %p\n", tv);
        delete tv;
#endif    
    }
    versionedTVs.clear();
}

void CompositeTruthValue::copy(CompositeTruthValue const& source) {
    //printf("CompositeTruthValue::copy()\n");
#ifdef USE_SHARED_DEFAULT_TV    
    primaryTV = (source.primaryTV == &(TruthValue::DEFAULT_TV())) ? (TruthValue*) &(TruthValue::DEFAULT_TV()) : source.primaryTV->clone();
#else
    primaryTV = source.primaryTV->clone();
    //printf("CompositeTruthValue::copy() => created new primaryTV = %p\n", primaryTV);
#endif    
    for (VersionedTruthValueMap::const_iterator itr = source.versionedTVs.begin();
          itr != source.versionedTVs.end(); itr++) {
        VersionHandle vh = itr->first;
        TruthValue* tv = itr->second;
#ifdef USE_SHARED_DEFAULT_TV        
        versionedTVs[vh] = (tv == &(TruthValue::DEFAULT_TV())) ? (TruthValue*) &(TruthValue::DEFAULT_TV()) : tv->clone();
#else 
//        versionedTVs[vh] = tv->clone();
        TruthValue* newTv = tv->clone();
        versionedTVs[vh] = newTv;
        //printf("CompositeTruthValue::copy() => created new versioned tv = %p\n", newTv);
#endif        
    }
}

CompositeTruthValue::CompositeTruthValue() {
    primaryTV = NULL;
}

CompositeTruthValue::CompositeTruthValue(const TruthValue& tv, VersionHandle vh) {
    init(tv, vh);
}

CompositeTruthValue::CompositeTruthValue(CompositeTruthValue const& source) {
    copy(source);
}

CompositeTruthValue::~CompositeTruthValue() {
    clear();
}

const TruthValue& CompositeTruthValue::getPrimaryTV() {
    return *primaryTV;
}

float CompositeTruthValue::getMean()  const {
    return primaryTV->getMean();
}

float CompositeTruthValue::getCount()  const {
    return primaryTV->getCount();
}


float CompositeTruthValue::getConfidence()  const {
    return primaryTV->getConfidence();
}

// Canonic methods 
float CompositeTruthValue::toFloat() const {
    // TODO: review this to consider versioned TVs as well?
    return primaryTV->toFloat();
}

/*
 * The format of string representation of a Composite TV is as follows (primaryTv first, followed by the versionedTvs):
 * {FIRST_PTL_TRUTH_VALUE;[0.000001,0.500000=0.000625]}{0x2;CONTEXTUAL;FIRST_PTL_TRUTH_VALUE;[0.500000,1.000000=0.001248]}
 * NOTE: string representation of tv types, VersionVandles and tv attributes cannot have '{', '}' or ';', 
 * which are separators
 */
std::string CompositeTruthValue::toString() const {
    //printf("CompositeTruthValue::toString()\n");
    std::string result;
    char buffer[1<<16];
    //printf("primaryTV = %p\n", primaryTV); 
    //printf("type = %d\n", primaryTV->getType()); 
    //printf("typeStr = %s\n", TruthValue::typeToStr(primaryTV->getType())); 
    //printf("{%s;%s}", TruthValue::typeToStr(primaryTV->getType()), primaryTV->toString().c_str()); 
    sprintf(buffer, "{%s;%s}", TruthValue::typeToStr(primaryTV->getType()), primaryTV->toString().c_str());
    result += buffer;
    for (VersionedTruthValueMap::const_iterator itr = versionedTVs.begin(); itr != versionedTVs.end(); itr++) {
        VersionHandle key = itr->first;
        TruthValue* tv = itr->second;
        //printf("{%p;%s;%s;%s}", key.substantive, VersionHandle::indicatorToStr(key.indicator), TruthValue::typeToStr(tv->getType()), tv->toString().c_str());
        sprintf(buffer, "{%lu;%s;%s;%s}", (unsigned long) key.substantive, VersionHandle::indicatorToStr(key.indicator), 
                                         TruthValue::typeToStr(tv->getType()), tv->toString().c_str());
        result += buffer;
    }

    //printf("\n%s\n", result.c_str());
    return result;
}

/*
 * The format of string representation of a Composite TV is as follows (primaryTv first, followed by the versionedTvs):
 * {FIRST_PTL_TRUTH_VALUE;[0.000001,0.500000=0.000625]}{0x2;CONTEXTUAL;FIRST_PTL_TRUTH_VALUE;[0.500000,1.000000=0.001248]}
 * NOTE: string representation of tv types, VersionHandles and tv attributes cannot have '{', '}' or ';', 
 * which are separators
 */

CompositeTruthValue* CompositeTruthValue::fromString(const char* tvStr) throw (InvalidParamException){
    char* buff;
    char* s = strdup(tvStr);
    // First each token is a tv representation
    char* tvToken = __strtok_r(s, "{}", &buff);
    if (tvToken == NULL) {
        throw InvalidParamException(TRACE_INFO, 
             "Invalid string representation of a CompositeTruthValue object: missing primary TV!");
    }
    // Creates the new instance.
    CompositeTruthValue* result = new CompositeTruthValue();
    // Now, separate internal tokens 
    char* internalBuff;
    char* primaryTvTypeStr = __strtok_r(tvToken,";",&internalBuff);
    TruthValueType primaryTvType = TruthValue::strToType(primaryTvTypeStr);
    char* primaryTvStr = __strtok_r(NULL,";",&internalBuff);
    //printf("tvTypeStr = %s, tvStr = %s\n", primaryTvTypeStr, primaryTvStr);
    result->primaryTV = TruthValue::factory(primaryTvType, primaryTvStr);
#ifdef USE_SHARED_DEFAULT_TV    
    DeleteAndSetDefaultTVIfPertinent(&(result->primaryTV));
#endif    
    
    // Get the versioned tvs
    while ((tvToken = __strtok_r(NULL, "{}", &buff)) != NULL) {
        char* substantiveStr = __strtok_r(tvToken,";",&internalBuff);
        Handle substantive;
        sscanf(substantiveStr, "%lu", (unsigned long *) &substantive);
        // TODO: IF THIS IS USED BY SAVING & LOADING, THIS HANDLE MUST BE CONVERTED TO A NEW/COMMON HANDLE FORMAT.
        char* indicatorStr = __strtok_r(NULL,";",&internalBuff);
        IndicatorType indicator = VersionHandle::strToIndicator(indicatorStr);
        //printf("substantive = %p, indicator = %d\n", substantive, indicator);
        char* versionedTvTypeStr = __strtok_r(NULL,";",&internalBuff);
        TruthValueType versionedTvType = TruthValue::strToType(versionedTvTypeStr);
        char* versionedTvStr = __strtok_r(NULL,";",&internalBuff);
        //printf("tvTypeStr = %s, tvStr = %s\n", versionedTvTypeStr, versionedTvStr);
        VersionHandle vh(indicator, substantive);
#ifdef USE_SHARED_DEFAULT_TV        
        TruthValue* tv = TruthValue::factory(versionedTvType, versionedTvStr);
        DeleteAndSetDefaultTVIfPertinent(&tv);
        result->versionedTVs[vh] = tv;
#else
        result->versionedTVs[vh] = TruthValue::factory(versionedTvType, versionedTvStr);
#endif        
    }
    free(s);
    return result;
}


CompositeTruthValue* CompositeTruthValue::clone() const {
    return new CompositeTruthValue(*this);
}

CompositeTruthValue& CompositeTruthValue::operator=(const TruthValue& rhs) throw (RuntimeException){
    //printf("CompositeTruthValue::operator=()\n");
    const CompositeTruthValue* tv = dynamic_cast<const CompositeTruthValue*>(&rhs);
    if (tv) {
        if (tv != this) { // check if this is the same object first.
            //printf("operator=() calling clear()\n");
            clear();
            //copy(*tv);
            //printf("operator=() calling copy()\n");
            copy((const CompositeTruthValue&) rhs);
        }
    } else {
#if 0	
		// The following line was causing a compilation error on MSVC...
        throw RuntimeException(TRACE_INFO, "Cannot assign a TV of type '%s' to one of type '%s'\n", 
                                    typeid(rhs).name(), typeid(*this).name());
#else
        throw RuntimeException(TRACE_INFO, "Invalid assignment of a CompositeTV object\n");
#endif
    }
    return *this;
}

CompositeTruthValue& CompositeTruthValue::operator=(const CompositeTruthValue& rhs) throw (RuntimeException){
    //printf("CompositeTruthValue::operator=(const CompositeTruthValue&)\n");
    return operator=((const TruthValue&) rhs);
}


TruthValueType CompositeTruthValue::getType() const {
    return COMPOSITE_TRUTH_VALUE;
}


TruthValue* CompositeTruthValue::merge(const TruthValue& other) const {
    CompositeTruthValue* result = clone();
#if 1  
    // TODO: Use the approach with dynamic cast bellow if we're going to have subclasses 
    // of CompositeTruthValue. For now, this approach using getType() is more efficient.
    if (other.getType() == COMPOSITE_TRUTH_VALUE) {
        const CompositeTruthValue* otherCTv = (CompositeTruthValue*) &other;
#else        
        const CompositeTruthValue *otherCTv = dynamic_cast<const CompositeTruthValue *>(&other);
    if (otherCTv) {
#endif        
        if (otherCTv->getConfidence() > result->getConfidence()) {
#ifdef USE_SHARED_DEFAULT_TV            
            result->setVersionedTV(*(otherCTv->primaryTV), NULL_VERSION_HANDLE);
#else
            delete result->primaryTV;
            result->primaryTV = otherCTv->primaryTV->clone();
#endif            
        }
        // merge the common versioned TVs
        for (VersionedTruthValueMap::const_iterator itr = result->versionedTVs.begin();
              itr != result->versionedTVs.end(); itr++) {
            VersionHandle key = itr->first;
            TruthValue* tv = itr->second;
            VersionedTruthValueMap::const_iterator otherItr = otherCTv->versionedTVs.find(key);
            if (otherItr != otherCTv->versionedTVs.end()) {
                TruthValue* otherTv = otherItr->second;
                if (otherTv->getConfidence() > tv->getConfidence()) {
#ifdef USE_SHARED_DEFAULT_TV            
                    result->setVersionedTV(*otherTv, key);
#else
                    delete tv;
                    result->versionedTVs[key] = otherTv->clone();
#endif                    
                }
            }
        }
        // adds the non-existing versioned TVs
        for (VersionedTruthValueMap::const_iterator otherItr = otherCTv->versionedTVs.begin();
              otherItr != otherCTv->versionedTVs.end(); otherItr++) {
            VersionHandle key = otherItr->first;
            TruthValue* otherTv = otherItr->second;
            VersionedTruthValueMap::const_iterator itr = result->versionedTVs.find(key);
            if (itr == result->versionedTVs.end()) {
#ifdef USE_SHARED_DEFAULT_TV            
                result->setVersionedTV(*otherTv, key);
#else
                result->versionedTVs[key] = otherTv->clone();
#endif                    
            }
        }
    } else {
        if (other.getConfidence() > result->getConfidence()) {
#ifdef USE_SHARED_DEFAULT_TV            
            result->setVersionedTV(other, NULL_VERSION_HANDLE);
#else
            delete result->primaryTV;
            result->primaryTV = other.clone();
#endif                    
        }
    }
    return result;
}


void CompositeTruthValue::setVersionedTV(const TruthValue& tv, VersionHandle vh) {

#ifdef USE_SHARED_DEFAULT_TV              
    TruthValue* newTv = (TruthValue*) &(TruthValue::DEFAULT_TV());
    if (!tv.isNullTv() && &tv != &(TruthValue::DEFAULT_TV())) {
#else
    TruthValue* newTv;
    if (tv.isNullTv()) {
        newTv = TruthValue::DEFAULT_TV().clone();
    } else {
#endif                    
        newTv = tv.clone();
    }
    VersionedTruthValueMap::const_iterator itr = versionedTVs.find(vh);
    if (itr == versionedTVs.end()) {
        if (!isNullVersionHandle(vh)) {
            // valid version handle
            versionedTVs[vh] = newTv;
        } else {
            // null version handle. Set the primary TV
#ifdef USE_SHARED_DEFAULT_TV
            if (primaryTV != NULL && primaryTV != &(TruthValue::DEFAULT_TV())) {
#else
            if (primaryTV != NULL) {
#endif                
                delete primaryTV;
            }
            primaryTV = newTv;
        }
    } else {
        TruthValue* versionedTv = itr->second;
#ifdef USE_SHARED_DEFAULT_TV
        if (versionedTv != &(TruthValue::DEFAULT_TV())) {
            delete versionedTv;
        }
        versionedTVs[vh] = newTv;
#else
        delete versionedTv;
        versionedTVs[vh] = newTv;
#endif

    }
}


const TruthValue& CompositeTruthValue::getVersionedTV(VersionHandle vh) const
{
    if (!isNullVersionHandle(vh)) {
        VersionedTruthValueMap::const_iterator itr = versionedTVs.find(vh);
        if (itr == versionedTVs.end()) {
            return TruthValue::NULL_TV();
        } else {
            return *(itr->second);
        }
    } else {
        return *primaryTV;
    }
}

void CompositeTruthValue::removeVersionedTV(VersionHandle vh) {
    VersionedTruthValueMap::const_iterator itr = versionedTVs.find(vh);
    if (itr != versionedTVs.end()) {
        TruthValue* versionedTv = itr->second;
        versionedTVs.erase(vh);
#ifdef USE_SHARED_DEFAULT_TV
        if (versionedTv != &(TruthValue::DEFAULT_TV())) {
            delete versionedTv;
        }
#else
        delete versionedTv;
#endif        
    }
}

void CompositeTruthValue::removeVersionedTVs(Handle substantive) {
    VersionedTruthValueMap toBeRemovedEntries;
    for (VersionedTruthValueMap::const_iterator itr = versionedTVs.begin();
          itr != versionedTVs.end(); itr++) {
        VersionHandle key = itr->first;
        if (!CoreUtils::compare(key.substantive, substantive)) {
            toBeRemovedEntries[key] = NULL;
            // Free TruthValue object at once
            TruthValue* versionedTv = itr->second;
#ifdef USE_SHARED_DEFAULT_TV
            if (versionedTv != &(TruthValue::DEFAULT_TV())) {
                delete versionedTv;
            }
#else
            delete versionedTv;
#endif            
        }
    }
    for (VersionedTruthValueMap::const_iterator itr = toBeRemovedEntries.begin();
          itr != toBeRemovedEntries.end(); itr++) {
        VersionHandle key = itr->first;
        versionedTVs.erase(key);
    }
}


int CompositeTruthValue::getNumberOfVersionedTVs() const {
    return versionedTVs.size();
}

VersionHandle CompositeTruthValue::getVersionHandle(int i) const {
    int index = 0;
    for (VersionedTruthValueMap::const_iterator itr = versionedTVs.begin();
          itr != versionedTVs.end(); itr++) {
        VersionHandle vh = itr->first;
        if (index++ == i) {
            return vh;
        }
    }
    return NULL_VERSION_HANDLE;
}


void CompositeTruthValue::updateVersionHandles(HandleMap *handles)
{
    VersionedTruthValueMap newVersionedTVs;
    for (VersionedTruthValueMap::const_iterator itr = versionedTVs.begin();
          itr != versionedTVs.end(); itr++) {
        VersionHandle key = itr->first;
        CoreUtils::updateHandle(&(key.substantive), handles);
        newVersionedTVs[key] = itr->second;
    }
    versionedTVs = newVersionedTVs;
}
