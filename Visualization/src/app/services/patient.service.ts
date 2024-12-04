import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { catchError, Observable, of } from 'rxjs';
import { EditPatient } from '../domain/edit-patient';
import { PatientQueryParameters } from '../domain/patient-query-parameters';
import { PatientWithId } from '../domain/patient-with-id';
import { EditPatientProfile } from '../domain/edit-patient-profile';
import { IdPasser } from '../domain/IdPasser';
import { Patient } from '../domain/Patient';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class PatientService {

  theServerURL = environment.serverBaseUrl + "/Patient"
  token = localStorage.getItem('user') ? JSON.parse(localStorage.getItem('user')!).token : null;
  httpOptions = {
    headers: new HttpHeaders({
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${this.token}`
    })
   };

  constructor(private messageService: MessageService, private http: HttpClient) { }
  public updateProfile(name: string, phone: string, email: string, address: string, emergencyContact: string) {
    const url = `${this.theServerURL}`;

    // Construct the patient object, including only fields that have values
    let patient: EditPatient = {};

    if (name && name.trim() !== "") {
      patient.name = name;
    }
    if (phone && phone.trim() !== "") {
      patient.phone = phone;
    }
    if (email && email.trim() !== "") {
      patient.email = email;
    }
    if (address && address.trim() !== "") {
      patient.address = address;
    }
    if (emergencyContact && emergencyContact.trim() !== "") {
      patient.emergencyContact = emergencyContact;
    }

    this.http.put<Patient>(url, patient, this.httpOptions)
      .pipe(catchError(this.handleError<Patient>('Update patient profile')))
      .subscribe(data => {
        this.log(`Patient profile was successfully updated.`);
      });
  }


  public createPatientProfile(firstName: string, lastName: string, phone: string, email: string, address: string, emergencyContact: string, gender: string, dateBirth: string) {
    const url = `${this.theServerURL}/Create-PatientProfile`;
    let patient: Patient = {
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      datebirth: dateBirth,
      emergencyContact: emergencyContact,
      gender: gender,
    };

    this.http.post<Patient>(url, patient, this.httpOptions)
      .pipe(catchError(this.handleError<Patient>('Create patient profile')))
      .subscribe(data => {
        this.log(`Patient profile: ${data.email} was successfully created.`);
      });
  }

  public editPatientProfile(id: string, name: string, phone: string, email: string, address: string, dateBirth: string) {
    const url = `${this.theServerURL}/${id}`;
    let editPatient: EditPatientProfile = {
      id: id
    };

    if (name && name.trim() !== "") {
      editPatient.name = name;
    }

    if (phone && phone.trim() !== "") {
      editPatient.phone = phone;
    }

    if (email && email.trim() !== "") {
      editPatient.email = email;
    }

    if (address && address.trim() !== "") {
      editPatient.address = address;
    }

    if (dateBirth && dateBirth.trim() !== "") {
      editPatient.dateBirth = dateBirth;
    }

    this.http.put<Patient>(url, editPatient, this.httpOptions)
      .pipe(catchError(this.handleError<EditPatientProfile>('Edited patient profile')))
      .subscribe(data => {
        this.log(`Patient profile: was successfully updated.`);
      });
  }


  getPatientsByFilters(patientQueryParameters: PatientQueryParameters): Observable<PatientWithId[]> {
    const url = `${this.theServerURL}/Filtered-List`;

    return this.http.post<PatientWithId[]>(url, patientQueryParameters, this.httpOptions).pipe(
        catchError((error) => {
          if (error.status = 404) {

            const queryParameters: PatientQueryParameters = {
              queryfilters: []
            };

            queryParameters.queryfilters.push(
              {
                firstName : '',
                lastName : '',
                email : '',
                gender : '',
                dateBirth : '',
                medicalRecordNumber : ''
              }
            )

            this.log('No patient profiles were found with the chosen criteria.')
            return this.http.post<PatientWithId[]>(url, queryParameters, this.httpOptions)
          }else {
            this.handleError<PatientWithId[]>('Get patient profile filtered list', error);
            return of([]);
          }
        })
    );
  }

  getPatientById(id: string): Observable<PatientWithId> {
    const url = `${this.theServerURL}/SimpleId/${id}`;

    return this.http.get<PatientWithId>(url, this.httpOptions).pipe(
        catchError((error) => {
            this.handleError<PatientWithId>('Get patient profile', error);
            return of({} as PatientWithId);
        })
    );
  }

  deactivatePatientProfile(patientId: string) {
    const url = `${this.theServerURL}/Delete-PatientProfile`;
    let idPasser: IdPasser = {
      id: patientId
    };

    this.http.put<{ message : string }>(url, idPasser, this.httpOptions)
      .pipe(catchError(this.handleError<{ message : string }>('Deactivate patient profile')))
      .subscribe(data => {
        this.log(`${data.message}`);
      });
  }



  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if (error.status === 200 || error.status === 400) {
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured. [${error.message}]`);
      }
      return of(result as T);
    };
  }

  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
